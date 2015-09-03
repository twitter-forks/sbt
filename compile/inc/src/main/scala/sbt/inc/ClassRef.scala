package sbt.inc

import sbt.IO
import java.io.File

/**
 * Represents a classfile on disk, which may be either loose in a directory, or inside a jar.
 */
sealed trait ClassRef

object ClassRef {
  case class Loose(file: File) extends ClassRef
  case class Jarred(jarFile: File, entry: String) extends ClassRef
}
