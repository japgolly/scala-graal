package japgolly.scalagraal

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import org.graalvm.polyglot.{Context, Engine}
import scala.concurrent.Future
import scala.concurrent.JavaConversions.asExecutionContext

// TODO Future... yuk -- although we *are* creating our own ExecutorService...
trait ContextPool extends ContextF[Future] {
  def poolSize: Int
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

  sealed trait OnShutdown
  object OnShutdown {
    case object CloseContexts extends OnShutdown
    case object DoNothing extends OnShutdown
  }

  def fixedThreadPool(poolSize: Int)(implicit l: Language): ContextPool =
    Builder.fixedThreadPool(poolSize).fixedContextPerThread().build()

//  def fixedThreadPool(poolSize: Int, f: Engine => ContextSync): ContextPool =
//    Builder.fixedThreadPool(poolSize).context(f).build()

  object Builder {
    def fixedThreadPool(poolSize: Int): Fixed.Step1 =
      new Fixed.Step1(poolSize)

    object Fixed {
      final class Step1(poolSize: Int) {

//        def context(f: Engine => ContextSync): Step2B =
//          new Step2B(poolSize, f)

        def fixedContextPerThread()(implicit l: Language): Step2A =
          fixedContextPerThread(l :: Nil)

        def fixedContextPerThread(ls: Seq[Language]): Step2A =
          new Step2A(
            poolSize,
            e => ContextSync.Builder.fixedContext(InternalUtils.newContext(ls.map(_.name), e)),
            OnShutdown.CloseContexts)

        def fixedContextPerThread(f: Engine => Context): Step2A =
          new Step2A(poolSize, e => ContextSync.Builder.fixedContext(f(e)), OnShutdown.CloseContexts)

        def newContextPerUse()(implicit l: Language): Step2A =
          newContextPerUse(l :: Nil)

        def newContextPerUse(ls: Seq[Language]): Step2A =
          newContextPerUse(InternalUtils.newContext(ls.map(_.name), _))

        def newContextPerUse(f: Engine => Context): Step2A =
          new Step2A(poolSize, e => ContextSync.Builder.newContextPerUse(f(e)), OnShutdown.DoNothing)
      }

      final class Step2A(poolSize: Int, perThread: Engine => ContextSync.Builder, onShutdown: OnShutdown) {
        def configure(f: ContextSync.Builder => ContextSync.Builder): Step2A =
          new Step2A(poolSize, f compose perThread, onShutdown)

        def build(): ContextPool =
          new Step2B(poolSize, perThread.andThen(_.build()), onShutdown).build()
      }

      final class Step2B(poolSize: Int, perThread: Engine => ContextSync, onShutdown: OnShutdown) {
        def build(): ContextPool = {
          val e = Engine.create()
          fixedPool(poolSize, () => perThread(e), onShutdown)
        }
      }
    }
  }

  private def fixedPool(poolSize: Int, newContext: () => ContextSync, onShutdown: OnShutdown): ContextPool = {
    val poolNo = poolCount.getAndIncrement()
    val threadCount = new AtomicInteger(1)
    fixedPool(poolSize, DefaultContextThread(poolNo, threadCount, newContext, _), onShutdown)
  }

  private def fixedPool(poolSize: Int, createNewThread: Runnable => ContextThread, onShutdown: OnShutdown): ContextPool = {
    val lock = new AnyRef
    var threads = List.empty[ContextThread]

    val threadFactory = new ThreadFactory {
      override def newThread(r: Runnable) = {
        val t = createNewThread(r)
        lock.synchronized(threads ::= t)
        t
      }
    }

    val executor = new ThreadPoolExecutor(
      poolSize, poolSize,
      0L, TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable],
      threadFactory)

    val shutdown = () => {
      executor.shutdown()
      executor.awaitTermination(900, TimeUnit.DAYS)
      onShutdown match {
        case OnShutdown.CloseContexts =>
          lock.synchronized(threads).foreach(_.contextSync.close())
        case OnShutdown.DoNothing =>
          ()
      }
    }

    executor.prestartAllCoreThreads()

    new ExecutorServiceBased(poolSize, executor, shutdown)
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

  private class ExecutorServiceBased(val poolSize: Int, es: ExecutorService, doShutdown: () => Unit) extends ContextPool {
    private[this] implicit val ec = asExecutionContext(es)

    private def contextSync(): ContextSync =
      Thread.currentThread().asInstanceOf[ContextThread].contextSync

    override def eval[A](expr: Expr[A], mw2: ContextMetrics.Writer) = {
      val startTime = DurationLite.start()
      Future {
        contextSync().evalT(expr, startTime, mw2)
      }
    }

    override def evalWithStats[A](expr: Expr[A], mw2: ContextMetrics.Writer) = {
      val startTime = DurationLite.start()
      Future {
        val cm = ContextMetrics.Writer.StoreLast()
        val er = contextSync().evalT(expr, startTime, mw2 >> cm)
        ContextMetrics.AndExprResult(cm.last, er)
      }
    }

    private val shutdownLock = new AnyRef
    override def shutdown(): Unit =
      shutdownLock.synchronized {
        poolState() match {
          case State.Active =>
            doShutdown()
          case State.ShuttingDown | State.Terminated =>
            ()
        }
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
