/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{ FileRef, FileRefJarred, FileRefLoose }

import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.io.{ AbstractFile, PlainFile, ZipArchive }

import scala.collection.JavaConverters._

import java.io.{ File, FileNotFoundException }
import java.net.URL
import java.util.zip.{ ZipEntry, ZipException, ZipFile }

/**
 * A mixin for compiler phases that do class lookups. Contains utility methods for looking up
 * class files corresponding to Symbols
 */
abstract class LocateClassFile extends Compat {
  val global: CallbackGlobal
  import global._

  /**
   * Builds and returns a current index of the classpath.
   */
  def classFileLocator() = new Locator

  def isTopLevelModule(sym: Symbol): Boolean =
    atPhase(currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }

  def className(s: Symbol, sep: Char, dollarRequired: Boolean): String =
    flatname(s, sep) + (if (dollarRequired) "$" else "")

  private def flatname(s: Symbol, separator: Char) =
    atPhase(currentRun.flattenPhase.next) { s fullName separator }

  protected def classFileName(s: Symbol, dollarRequired: Boolean): String =
    className(s, File.separatorChar, dollarRequired) + ".class"

  protected def classFileName(name: String): String =
    name.replace('.', '/') + ".class"

  /**
   * An index of classfile inputs and outputs. Should be reinstantiated whenever it
   * is expected that outputs have changed.
   */
  protected class Locator {
    private[this] final val classSeparator = '.'
    def classFile(sym: Symbol): Option[(FileRef, String, Boolean)] =
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

    def findOnClassPath(name: String): Option[FileRef] =
      // TODO: worth indexing the compiler's classpath to minimize object creation?
      classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]]).flatMap {
        case ze: ZipArchive#Entry =>
          for (zip <- ze.underlyingSource; zipFile <- Option(zip.file)) yield {
            new FileRefJarred(zipFile, ze.path)
          }
        case pf: PlainFile =>
          Some(new FileRefLoose(pf.file))
        case _ =>
          None
      }

    def getOutputClass(sym: Symbol, separatorRequired: Boolean): Option[FileRef] =
      getOutputClassForFilename(classFileName(sym, separatorRequired))

    def findClass(name: String): Option[(FileRef, Boolean)] =
      getOutputClass(name).map(f => (f, true)) orElse findOnClassPath(name).map(f => (f, false))

    def getOutputClass(name: String): Option[FileRef] =
      getOutputClassForFilename(classFileName(name))

    def getOutputClassForFilename(filename: String): Option[FileRef] =
      outputJarContents.collectFirst {
        // scan jars first since they're indexed
        case (jarFile, classFiles) if classFiles(filename) =>
          new FileRefJarred(jarFile, filename)
      }.orElse {
        // scan directories
        outputDirectories.iterator.map(new File(_, filename)).find(_.exists()).map { file =>
          new FileRefLoose(file)
        }
      }

    /** @return An index of output jar Files to the filenames they contain. */
    private[this] val outputJarContents: Map[File, Set[String]] =
      outputJars.flatMap { jol =>
        // TODO: from sbt.inc.Locate
        val jarOpt =
          try {
            Some(new ZipFile(jol.file, ZipFile.OPEN_READ))
          } catch {
            case e: FileNotFoundException =>
              None
            case e: ZipException =>
              throw new RuntimeException("Error opening zip file: " + jol.file, e)
          }
        jarOpt.map { jar =>
          try {
            def fileEntries = (jar.entries.asScala: Iterator[ZipEntry]).filterNot(_.isDirectory)
            jol.file -> fileEntries.map(_.getName).toSet
          } finally {
            jar.close()
          }
        }
      }.toMap
  }
}
