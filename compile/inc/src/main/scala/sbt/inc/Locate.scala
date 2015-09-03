/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import java.io.File
import java.util.zip.{ ZipException, ZipFile }
import Function.const

object Locate {
  type DefinesClass = File => String => Option[ClassRef]

  /**
   * Right(src) provides the value for the found class
   * Left(true) means that the class was found, but it had no associated value
   * Left(false) means that the class was not found
   * def value[S](classpath: Seq[File], get: File => String => Option[S]): String => Either[Boolean, S] =
   * {
   * val gets = classpath.toStream.map(getValue(get))
   * className => find(className, gets)
   * }
   *
   * def find[S](name: String, gets: Stream[String => Either[Boolean, S]]): Either[Boolean, S] =
   * if (gets.isEmpty)
   * Left(false)
   * else
   * gets.head(name) match {
   * case Left(false) => find(name, gets.tail)
   * case x           => x
   * }
   */

  /**
   * Returns a function that searches the provided class path for
   * a class name and returns the entry that defines that class.
   * def entry(classpath: Seq[File], f: DefinesClass): String => Option[ClassRef] = {
   * val entries =
   * classpath.map { file =>
   * (file, f(file))
   * }.toStream
   * def fn(className: String): Option[ClassRef] =
   * entries.flatMap {
   * case (file, defines) => defines(className)
   * }.headOption
   * fn
   * }
   *
   * def getValue[S](get: File => String => Option[S])(entry: File): String => Either[Boolean, S] =
   * {
   * val defClass = definesClass(entry)
   * val getF = get(entry)
   * className => if (defClass(className)) getF(className).toRight(true) else Left(false)
   * }
   */

  def definesClass(entry: File): String => Option[ClassRef] =
    if (entry.isDirectory)
      directoryDefinesClass(entry)
    else if (entry.exists && classpath.ClasspathUtilities.isArchive(entry, contentFallback = true))
      jarDefinesClass(entry)
    else
      const(None)

  def jarDefinesClass(jarEntry: File): String => Option[ClassRef.Jarred] =
    {
      import collection.JavaConversions._
      val jar = try { new ZipFile(jarEntry, ZipFile.OPEN_READ) } catch {
        // ZipException doesn't include the file name :(
        case e: ZipException => throw new RuntimeException("Error opening zip file: " + jarEntry.getName, e)
      }
      val entries: Map[String, ClassRef.Jarred] =
        try {
          jar.entries.map { e =>
            toClassName(e.getName, '/') -> ClassRef.Jarred(jarEntry, e.getName)
          }.toMap
        } finally {
          jar.close()
        }
      entries.get _
    }

  def fromClassName(entry: String, separator: Char = File.separatorChar): String =
    entry.replace('.', separator) + ClassExt

  def toClassName(entry: String, separator: Char = File.separatorChar): String =
    entry.stripSuffix(ClassExt).replace(separator, '.')

  val ClassExt = ".class"

  def directoryDefinesClass(entry: File): String => Option[ClassRef.Loose] = { className =>
    val path = new File(entry, fromClassName(className))
    if (path.isFile)
      Some(ClassRef.Loose(path))
    else
      None
  }
}
