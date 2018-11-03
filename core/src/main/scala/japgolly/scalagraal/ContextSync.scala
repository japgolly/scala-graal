package japgolly.scalagraal

import org.graalvm.polyglot.Context

trait ContextSync {
  def apply[A](f: Context => A): A
  def withAround(f: ContextSync.Around): ContextSync
}

object ContextSync {

  def single(context: Context, mutex: Boolean = true): ContextSync =
    new Single(context, if (mutex) new AnyRef else null, Around.id)

  private class Single(context: Context, lock: AnyRef, around: Around) extends ContextSync {
    override def apply[A](f: Context => A) =
      if (lock eq null)
        around(context, f)
      else
        lock.synchronized(around(context, f))

    override def withAround(f: ContextSync.Around) =
      new Single(context, lock, around.insideOf(f))
  }

  // ===================================================================================================================

  trait Around { inner =>
    def apply[A](c: Context, f: Context => A): A
    def insideOf(outer: Around): Around = new Around {
      override def apply[A](c: Context, f: Context => A) =
        outer(c, inner(_, f))
    }
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

