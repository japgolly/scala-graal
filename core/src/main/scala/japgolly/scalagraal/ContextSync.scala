package japgolly.scalagraal

import org.graalvm.polyglot.Context

trait ContextSync {
  def apply[A](f: Context => A): A
  def withAround(f: ContextSync.Around): ContextSync
}

object ContextSync {

  def single(context: Context, mutex: Boolean = true): ContextSync =
    if (mutex)
      new SingleBlocking(context, Around.id, new AnyRef)
    else
      new SingleThreaded(context, Around.id)

  private class SingleBlocking(context: Context, around: Around, lock: AnyRef) extends ContextSync {
    override def apply[A](f: Context => A) =
      lock.synchronized {
        around.enterAndLeave(context, f)
      }

    override def withAround(f: ContextSync.Around) =
      new SingleBlocking(context, around.insideOf(f), lock)
  }

  private class SingleThreaded(context: Context, around: Around) extends ContextSync {
    override def apply[A](f: Context => A) =
      around.enterAndLeave(context, f)

    override def withAround(f: ContextSync.Around) =
      new SingleThreaded(context, around.insideOf(f))
  }

  // ===================================================================================================================

  trait Around { inner =>
    def apply[A](c: Context, f: Context => A): A

    def insideOf(outer: Around): Around = new Around {
      override def apply[A](c: Context, f: Context => A) =
        outer(c, inner(_, f))
    }

    final def enterAndLeave[A](c: Context, f: Context => A): A =
      try {
        c.enter()
        this(c, f)
      } finally
        c.leave()
  }

  object Around {
    val id: Around = new Around {
      override def apply[A](c: Context, f: Context => A) = f(c)
    }

    def before(h: Context => Unit): Around =
      new Around {
        override def apply[A](c: Context, f: Context => A): A = {
          h(c)
          f(c)
        }
      }

    def after(h: Context => Unit): Around =
      new Around {
        override def apply[A](c: Context, f: Context => A): A = {
          val a = f(c)
          h(c)
          a
        }
      }
  }
}

