package japgolly.scalagraal.util

import java.io.File
import java.nio.file.Path

trait FileRef[-A] {
  def str: A => String
  def file: A => File
  def path: A => Path
}

object FileRef {

  implicit val fromString: FileRef[String] =
    new FileRef[String] {
      override def str = identity
      override def file = new File(_)
      override def path = Path.of(_)
    }

  implicit val fromFile: FileRef[File] =
    new FileRef[File] {
      override def str = _.getAbsolutePath()
      override def file = identity
      override def path = _.toPath()
    }

  implicit val fromPath: FileRef[Path] =
    new FileRef[Path] {
      override def str = file(_).getAbsolutePath()
      override def file = _.toFile()
      override def path = identity
    }
}
