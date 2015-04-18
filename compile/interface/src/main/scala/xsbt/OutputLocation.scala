/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import java.io.File

/**
 * Trait to differentiate Jar outputs from Directory outputs.
 */
sealed trait OutputLocation {
  def file: File
  def prepare(): Unit
}
object OutputLocation {
  /**
   * @return Creates an OutputLocation for the given file, which will represent either
   * a jar or directory.
   */
  def apply(file: File) =
    if (endsWithJar(file)) {
      Jar(file)
    } else {
      Directory(file)
    }

  case class Jar(file: File) extends OutputLocation {
    assert(endsWithJar(file), s"${file} is not a jarfile.")
    def prepare() = IO.createDirectory(file.getParentFile)
  }
  case class Directory(file: File) extends OutputLocation {
    assert(!endsWithJar(file), s"${file} is (probably) not a directory.")
    def prepare() = IO.createDirectory(file)
  }

  private def endsWithJar(file: File): Boolean =
    file.getName.toLowerCase.endsWith(".jar")
}
