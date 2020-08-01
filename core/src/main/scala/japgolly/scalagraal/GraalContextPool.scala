package japgolly.scalagraal

import japgolly.scalagraal.ScalaGraalEffect.{Async, AsyncES}
import japgolly.scalagraal.util.DurationLite
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Future => _, _}
import org.graalvm.polyglot.{Context, Engine}
import scala.concurrent.Future

trait GraalContextPool[F[_]] extends GraalContextF[F] {
  val poolSize: Int

  /** unsafe because impure */
  def unsafeShutdown(): Unit

  /** unsafe because impure */
  def unsafePoolState(): GraalContextPool.State
}

object GraalContextPool {

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

  def fixedThreadPool(poolSize: Int)(implicit l: Language): GraalContextPool[Future] =
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

        def fixedContextPerThread()(implicit l: Language): Step2A[Future] =
          fixedContextPerThread(l :: Nil)

        def fixedContextPerThread(ls: Seq[Language]): Step2A[Future] =
          new Step2A[Future](
            poolSize,
            e => GraalContext.Builder.fixedContext(InternalUtils.newContext(ls.map(_.name), e)),
            OnShutdown.CloseContexts)

        def fixedContextPerThread(f: Engine => Context): Step2A[Future] =
          new Step2A[Future](poolSize, e => GraalContext.Builder.fixedContext(f(e)), OnShutdown.CloseContexts)

        def newContextPerUse()(implicit l: Language): Step2A[Future] =
          newContextPerUse(l :: Nil)

        def newContextPerUse(ls: Seq[Language]): Step2A[Future] =
          newContextPerUse(InternalUtils.newContext(ls.map(_.name), _))

        def newContextPerUse(f: Engine => Context): Step2A[Future] =
          new Step2A[Future](poolSize, e => GraalContext.Builder.newContextPerUse(f(e)), OnShutdown.DoNothing)
      }

      final class Step2A[F[_]: AsyncES](poolSize: Int, perThread: Engine => GraalContext.Builder, onShutdown: OnShutdown) {
        def configure(f: GraalContext.Builder => GraalContext.Builder): Step2A[F] =
          new Step2A(poolSize, f compose perThread, onShutdown)

        def resultType[G[_]: AsyncES]: Step2A[G] =
          new Step2A(poolSize, perThread, onShutdown)

        def awaitResultsWithTimeout(timeout: Long, timeUnit: TimeUnit) =
          resultType(AsyncES.syncTimed(timeout, timeUnit))

        def build(): GraalContextPool[F] =
          new Step2B(poolSize, perThread.andThen(_.build()), onShutdown).build()
      }

      final class Step2B[F[_]: AsyncES](poolSize: Int, perThread: Engine => GraalContext, onShutdown: OnShutdown) {
        def resultType[G[_]: AsyncES]: Step2B[G] =
          new Step2B(poolSize, perThread, onShutdown)

        def awaitResultsWithTimeout(timeout: Long, timeUnit: TimeUnit) =
          resultType(AsyncES.syncTimed(timeout, timeUnit))

        def build(): GraalContextPool[F] = {
          val e = Engine.create()
          fixedPool(poolSize, () => perThread(e), onShutdown)
        }
      }
    }
  }

  private def fixedPool[F[_]: AsyncES](poolSize: Int, newContext: () => GraalContext, onShutdown: OnShutdown): GraalContextPool[F] = {
    val poolNo = poolCount.getAndIncrement()
    val threadCount = new AtomicInteger(1)
    fixedPool(poolSize, DefaultContextThread(poolNo, threadCount, newContext, _), onShutdown)
  }

  private def fixedPool[F[_]](poolSize: Int, createNewThread: Runnable => ContextThread, onShutdown: OnShutdown)
                             (implicit F: AsyncES[F]): GraalContextPool[F] = {
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

    implicit val async = F(executor)

    new ExecutorServiceBased(poolSize, executor, shutdown)
  }

  private val poolCount = new AtomicInteger(1)
  private def threadGroup = new ThreadGroup("scalagraal")

  trait ContextThread extends Thread {
    def contextSync: GraalContext
  }

  private class DefaultContextThread(init: Runnable, name: String) extends Thread(threadGroup, init, name) with ContextThread {
    setDaemon(true)

    private[DefaultContextThread] var _contextSync: GraalContext = null
    override def contextSync = _contextSync
  }

  private object DefaultContextThread {
    def apply(poolNo: Int, poolThreads: AtomicInteger, newContext: () => GraalContext, init: Runnable): DefaultContextThread = {
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

  private class ExecutorServiceBased[F[_]](val poolSize: Int, es: ExecutorService, doShutdown: () => Unit)
                                          (implicit F: Async[F]) extends GraalContextPool[F] {

    private def contextSync(): GraalContext =
      Thread.currentThread().asInstanceOf[ContextThread].contextSync

    override def eval[A](expr: Expr[A], mw2: GraalContextMetrics.Writer) = {
      val startTime = DurationLite.start()
      F.delay {
        contextSync().evalT(expr, startTime, mw2)
      }
    }

    override def evalWithStats[A](expr: Expr[A], mw2: GraalContextMetrics.Writer) = {
      val startTime = DurationLite.start()
      F.delay {
        val cm = GraalContextMetrics.Writer.StoreLast()
        val er = contextSync().evalT(expr, startTime, mw2 >> cm)
        GraalContextMetrics.AndExprResult(cm.last, er)
      }
    }

    private val shutdownLock = new AnyRef
    override def unsafeShutdown(): Unit =
      shutdownLock.synchronized {
        unsafePoolState() match {
          case State.Active =>
            doShutdown()
          case State.ShuttingDown | State.Terminated =>
            ()
        }
        es.shutdown()
      }

    override def unsafePoolState(): State =
      if (es.isTerminated)
        State.Terminated
      else if (es.isShutdown)
        State.ShuttingDown
      else
        State.Active
  }
}
