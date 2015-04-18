/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.io.AbstractFile

import scala.collection.JavaConverters._

import java.io.File
import java.net.URL
import java.util.zip.{ ZipEntry, ZipException, ZipFile }

/**
 * An index of classfile inputs and outputs containing utility methods for looking up
 * class files corresponding to Symbols. Should be reinstantiated whenever it
 * is expected that outputs have changed.
 */
case class ClassFileLocator[G <: CallbackGlobal](global: G) extends Compat[G] {
  import global._

  /** @return An index of output jar Files to the filenames they contain. */
  private[this] val outputJarContents: Map[URL, Set[String]] =
    outputJars.map { jol =>
      // TODO: from sbt.inc.Locate
      val jar = try { new ZipFile(jol.file, ZipFile.OPEN_READ) } catch {
        // ZipException doesn't include the file name :(
        case e: ZipException => throw new RuntimeException("Error opening zip file: " + jol.file, e)
      }
      try {
        jol.file.toURI.toURL -> (jar.entries.asScala: Iterator[ZipEntry]).map(_.getName).toSet
      } finally {
        jar.close()
      }
    }.toMap

  private[this] final val classSeparator = '.'
  def classFile(sym: G#Symbol): Option[(AbstractFile, String, Boolean)] =
    // package can never have a corresponding class file; this test does not
    // catch package objects (that do not have this flag set)
    if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None else {
      import scala.tools.nsc.symtab.Flags
      val name = flatname(sym, classSeparator) + moduleSuffix(sym)
      findClass(name).map { case (file, inOut) => (file, name, inOut) } orElse {
        if (isTopLevelModule(sym)) {
          val linked = sym.companionClass
          if (linked == NoSymbol)
            None
          else
            classFile(linked)
        } else
          None
      }
    }

  def isTopLevelModule(sym: G#Symbol): Boolean =
    atPhase(currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }

  def findClass(name: String): Option[(AbstractFile, Boolean)] =
    getOutputClass(name).map(f => (f, true)) orElse findOnClassPath(name).map(f => (f, false))

  def getOutputClass(name: String): Option[AbstractFile] =
    getOutputClassURLForFilename(classFileName(name)).map(AbstractFile.getURL)

  def getOutputClass(sym: G#Symbol, separatorRequired: Boolean): Option[AbstractFile] =
    getOutputClassURL(sym, separatorRequired).map(AbstractFile.getURL)

  def getOutputClassURL(sym: G#Symbol, separatorRequired: Boolean): Option[URL] =
    getOutputClassURLForFilename(classFileName(sym, separatorRequired))

  def getOutputClassURLForFilename(filename: String): Option[URL] =
    outputJarContents.collect {
      // scan jars first since they're indexed
      case (jarURI, classFiles) if classFiles(filename) => jarURI
    }.headOption.map { jarURI =>
      new URL(jarURI + "/!/" + filename)
    }.orElse {
      // scan directories
      outputDirectories.map(new File(_, filename)).find(_.exists()).map(_.toURL)
    }

  def findOnClassPath(name: String): Option[AbstractFile] =
    classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]])

  def className(s: G#Symbol, sep: Char, dollarRequired: Boolean): String =
    flatname(s, sep) + (if (dollarRequired) "$" else "")

  private def flatname(s: G#Symbol, separator: Char) =
    atPhase(currentRun.flattenPhase.next) { s fullName separator }

  protected def classFileName(s: G#Symbol, dollarRequired: Boolean): String =
    className(s, File.separatorChar, dollarRequired) + ".class"

  protected def classFileName(name: String): String =
    name.replace('.', '/') + ".class"
}
