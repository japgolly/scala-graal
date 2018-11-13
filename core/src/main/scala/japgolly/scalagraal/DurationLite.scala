package japgolly.scalagraal

final case class DurationLite(nanos: Long) extends AnyVal {
  override def toString = "%,d ns".format(nanos)
  def toStrUs = "%,d us".format(nanos / 1000)
  def toStrMs = "%,d ms".format(nanos / 1000000)
  def +(b: DurationLite) = DurationLite(nanos + b.nanos)
}

object DurationLite {
  val Neg = apply(-1)
  val Zero = apply(0)

  final class StartTime(private val nanos: Long) extends AnyVal {
    def stop() = DurationLite(System.nanoTime() - nanos)
  }

  def start() = new StartTime(System.nanoTime())
}

