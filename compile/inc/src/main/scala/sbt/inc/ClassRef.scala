package sbt.inc

import sbt.IO
import java.io.File

case class ClassRefLoose(classFile: File) extends xsbti.ClassRefLoose {
  def containingFile = classFile
}

case class ClassRefJarred(jarFile: File, classFile: String) extends xsbti.ClassRefJarred {
  def containingFile = jarFile
}
