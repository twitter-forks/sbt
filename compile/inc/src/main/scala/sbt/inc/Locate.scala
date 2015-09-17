/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import xsbti.{ FileRef, FileRefJarred, FileRefLoose }
import java.io.{ IOException, File }
import java.util.jar.JarFile

object Locate {
  type DefinesClass = File => String => Option[FileRef]

  /**
   * Returns a function that searches the provided classpath for
   * a class name and returns the entry that defines that class.
   */
  def entry(classpath: Seq[File], f: DefinesClass): String => Option[FileRef] = {
    val entries =
      classpath.map { file =>
        (file, f(file))
      }.toStream
    def fn(className: String): Option[FileRef] =
      entries.flatMap {
        case (file, defines) => defines(className)
      }.headOption
    fn
  }

  def definesClass(entry: File): String => Option[FileRef] =
    if (entry.isDirectory)
      directoryDefinesClass(entry)
    else if (entry.exists && classpath.ClasspathUtilities.isArchive(entry, contentFallback = true))
      jarDefinesClass(entry)
    else
      Function.const(None)

  def jarDefinesClass(jarFile: File): String => Option[FileRefJarred] =
    {
      import collection.JavaConverters._
      val entries: Map[String, FileRefJarred] =
        try {
          new JarFile(jarFile).entries.asScala.map { e =>
            toClassName(e.getName, '/') -> new FileRefJarred(jarFile, e.getName)
          }.toMap
        } catch {
          case e: IOException =>
            throw new RuntimeException("Error indexing zip file: " + jarFile, e)
        }
      entries.get _
    }

  def fromClassName(entry: String, separator: Char = File.separatorChar): String =
    entry.replace('.', separator) + ClassExt

  def toClassName(entry: String, separator: Char = File.separatorChar): String =
    entry.stripSuffix(ClassExt).replace(separator, '.')

  val ClassExt = ".class"

  def directoryDefinesClass(entry: File): String => Option[FileRefLoose] = { className =>
    val path = new File(entry, fromClassName(className))
    if (path.isFile)
      Some(new FileRefLoose(path))
    else
      None
  }
}
