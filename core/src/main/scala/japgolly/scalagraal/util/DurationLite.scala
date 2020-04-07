package japgolly.scalagraal.util

import java.util.concurrent.TimeUnit

final case class DurationLite(nanos: Long) extends AnyVal {
  override def toString =
    if (nanos < 1000)       toStrNs else
    if (nanos < 1000000)    toStrUs else
    if (nanos < 1000000000) toStrMs else
    toStrSec

  def +(b: DurationLite) = DurationLite(nanos + b.nanos)
  def -(b: DurationLite) = DurationLite(nanos - b.nanos)
  def *(n: Double) = DurationLite((nanos.toDouble * n).toLong)
  def /(n: Double) = DurationLite((nanos.toDouble / n).toLong)

  def micros : Double = nanos.toDouble / 1000.0
  def millis : Double = nanos.toDouble / 1000000.0
  def seconds: Double = nanos.toDouble / 1000000000.0

  def toStrNs = "%,d ns".format(nanos)
  def toStrUs = "%,d us".format(nanos / 1000)
  def toStrMs = "%,d ms".format(nanos / 1000000)
  def toStrSec = "%,.1f sec".format(seconds)

  def asJava = java.time.Duration.ofNanos(nanos)
  def asScala = scala.concurrent.duration.FiniteDuration(nanos, TimeUnit.NANOSECONDS)
}

object DurationLite {
  val Neg = apply(-1)
  val Zero = apply(0)

  final class StartTime(private val nanos: Long) extends AnyVal {
    def stop() = DurationLite(System.nanoTime() - nanos)
  }

  def start() = new StartTime(System.nanoTime())

  def timeAndDiscardResult(a: => Any): DurationLite = {
    val t = start()
    a
    t.stop()
  }
}

