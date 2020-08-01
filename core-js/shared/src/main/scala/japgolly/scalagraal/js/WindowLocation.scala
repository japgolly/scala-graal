package japgolly.scalagraal.js

/** This corresponds to `window.location` in JavaScript.
  */
final case class WindowLocation(
    href    : String,
    origin  : String,
    protocol: String,
    hostname: String,
    port    : String,
    pathname: String,
    search  : String,
    hash    : String)

object WindowLocation {
  private[this] val regex = "^(.+?:)//(.+?)(?::([0-9]+))?(/.*?)?(\\?.*?)?(#.*)?$".r

  def parse(url: String): Option[WindowLocation] =
    url match {
      case regex(protocol, hostname, port0, path0, search0, hash0) =>
        val port   = if (port0 eq null) "" else port0
        val path   = if (path0 eq null) "" else path0
        val search = if (search0 eq null) "" else search0
        val hash   = if (hash0 eq null) "" else hash0
        val host   = if (port0 eq null) hostname else hostname + ":" + port0
        val origin = protocol + "//" + host

        Some(WindowLocation(
          href     = url     ,
          origin   = origin  ,
          protocol = protocol,
          hostname = hostname,
          port     = port    ,
          pathname = path    ,
          search   = search  ,
          hash     = hash    ))

      case _ => None
    }
}
