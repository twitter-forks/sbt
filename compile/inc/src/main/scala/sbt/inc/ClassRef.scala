package sbt.inc

import sbt.IO

/**
 * Represents a classfile on disk, which may be either loose in a directory, or inside a jar.
 */
sealed trait ClassRef

object ClassRef {
  case class Loose(path: String) extends ClassRef
  case class Jarred(jarPath: String, path: String) extends ClassRef
}
