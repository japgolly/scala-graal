package japgolly.scalagraal

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import org.graalvm.polyglot.{Context, Engine}
import scala.concurrent.Future
import scala.concurrent.JavaConversions.asExecutionContext

trait ContextPool extends ContextAsync {
  def shutdown(): Unit
  def poolState(): ContextPool.State
}

object ContextPool {

  sealed trait State
  object State {
    case object Active       extends State
    case object ShuttingDown extends State
    case object Terminated   extends State
  }

  def fixedThreadPool(poolSize: Int)(implicit l: Language): ContextPool =
    Builder.fixedThreadPool(poolSize).fixedContextPerThread().build()

  def fixedThreadPool(poolSize: Int, f: Engine => ContextSync): ContextPool =
    Builder.fixedThreadPool(poolSize).context(f).build()

  object Builder {
    def fixedThreadPool(poolSize: Int): Fixed.Step1 =
      new Fixed.Step1(poolSize)

    object Fixed {
      final class Step1(poolSize: Int) {

        def context(f: Engine => ContextSync): Step2B =
          new Step2B(poolSize, f)

        def fixedContextPerThread()(implicit l: Language): Step2A =
          fixedContextPerThread(l :: Nil)

        def fixedContextPerThread(ls: Seq[Language]): Step2A =
          new Step2A(poolSize, e => ContextSync.Builder.fixedContext(
            Context.newBuilder(ls.map(_.name): _*).engine(e).build()))

        def fixedContextPerThread(f: Engine => Context): Step2A =
          new Step2A(poolSize, e => ContextSync.Builder.fixedContext(f(e)))

        def newContextPerUse()(implicit l: Language): Step2A =
          newContextPerUse(l :: Nil)

        def newContextPerUse(ls: Seq[Language]): Step2A =
          newContextPerUse(Context.newBuilder(ls.map(_.name): _*).engine(_).build())

        def newContextPerUse(f: Engine => Context): Step2A =
          new Step2A(poolSize, e => ContextSync.Builder.newContextPerUse(f(e)))
      }

      final class Step2A(poolSize: Int, perThread: Engine => ContextSync.Builder) {
        def configure(f: ContextSync.Builder => ContextSync.Builder): Step2A =
          new Step2A(poolSize, f compose perThread)

        def build(): ContextPool =
          new Step2B(poolSize, perThread.andThen(_.build())).build()
      }

      final class Step2B(poolSize: Int, perThread: Engine => ContextSync) {
        def build(): ContextPool = {
          val e = Engine.create()
          fixedPool(poolSize, () => perThread(e))
        }
      }
    }
  }


  private def fixedPool(poolSize: Int, newContext: () => ContextSync): ContextPool = {
    val poolNo = poolCount.getAndIncrement()
    val threadCount = new AtomicInteger(1)
    fixedPool(poolSize, DefaultContextThread(poolNo, threadCount, newContext, _))
  }

  private def fixedPool(poolSize: Int, createNewThread: Runnable => ContextThread): ContextPool = {
    val threadFactory = new ThreadFactory {
      override def newThread(r: Runnable) = createNewThread(r)
    }

    val executor = new ThreadPoolExecutor(
      poolSize, poolSize,
      0L, TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable],
      threadFactory)

    executor.prestartAllCoreThreads()

    new ExecutorServiceBased(executor)
  }

  private val poolCount = new AtomicInteger(1)
  private def threadGroup = new ThreadGroup("scalagraal")

  trait ContextThread extends Thread {
    def contextSync: ContextSync
  }

  private class DefaultContextThread(init: Runnable, name: String) extends Thread(threadGroup, init, name) with ContextThread {
    setDaemon(true)

    private[DefaultContextThread] var _contextSync: ContextSync = null
    override def contextSync = _contextSync
  }

  private object DefaultContextThread {
    def apply(poolNo: Int, poolThreads: AtomicInteger, newContext: () => ContextSync, init: Runnable): DefaultContextThread = {
      val init2 = new Runnable {
        override def run(): Unit = {
          val self = Thread.currentThread().asInstanceOf[DefaultContextThread]
          self._contextSync = newContext()
          init.run()
        }
      }
      val name = s"ScalaGraal-pool-$poolNo-thread-${poolThreads.getAndIncrement()}"
      new DefaultContextThread(init2, name)
    }
  }

  private class ExecutorServiceBased(es: ExecutorService) extends ContextPool {
    private[this] implicit val ec = asExecutionContext(es)

    override def eval[A](expr: Expr[A]): Future[Expr.Result[A]] = {
      val startTime = DurationLite.start()
      Future {
        val t = Thread.currentThread().asInstanceOf[ContextThread]
        t.contextSync.evalT(startTime, expr)
      }
    }

    private val shutdownLock = new AnyRef
    override def shutdown(): Unit =
      shutdownLock.synchronized {
        // TODO check state first
        // TODO Close all contexts
        es.shutdown()
      }

    override def poolState(): State =
      if (es.isTerminated)
        State.Terminated
      else if (es.isShutdown)
        State.ShuttingDown
      else
        State.Active
  }
}
