/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010  Mark Harrah
 */
package sbt

import java.io.{ File, Writer }
import java.net.URL
import inc.Relations

object DotGraph {
  private def fToString(roots: Iterable[File]): (File => String) =
    (x: File) => sourceToString(roots, x)
  val u2f: URL => File = (u: URL) => new File(u.getPath)
  def sources(relations: Relations, outputDirectory: File, sourceRoots: Iterable[File]): Unit = {
    val f2s: File => String = fToString(sourceRoots)
    apply(relations, outputDirectory, f2s, f2s compose u2f)
  }
  def packages(relations: Relations, outputDirectory: File, sourceRoots: Iterable[File]): Unit = {
    val packageOnly = (path: String) =>
      {
        val last = path.lastIndexOf(File.separatorChar)
        val packagePath = (if (last > 0) path.substring(0, last) else path).trim
        if (packagePath.isEmpty) "" else packagePath.replace(File.separatorChar, '.')
      }
    val f2s = packageOnly compose fToString(sourceRoots)
    apply(relations, outputDirectory, f2s, f2s compose u2f)
  }
  def apply(relations: Relations, outputDir: File, sourceToString: File => String, externalToString: URL => String): Unit = {
    def file(name: String) = new File(outputDir, name)
    IO.createDirectory(outputDir)
    generateGraph(file("int-source-deps"), "dependencies", relations.internalSrcDep, sourceToString, sourceToString)
    generateGraph(file("binary-dependencies"), "externalDependencies", relations.binaryDep, sourceToString, externalToString)
  }

  def generateGraph[Key, Value](file: File, graphName: String, relation: Relation[Key, Value],
    keyToString: Key => String, valueToString: Value => String) {
    import scala.collection.mutable.{ HashMap, HashSet }
    val mappedGraph = new HashMap[String, HashSet[String]]
    for ((key, values) <- relation.forwardMap; keyString = keyToString(key); value <- values)
      mappedGraph.getOrElseUpdate(keyString, new HashSet[String]) += valueToString(value)

    val mappings =
      for {
        (dependsOn, dependants) <- mappedGraph.toSeq
        dependant <- dependants
        if dependant != dependsOn && !dependsOn.isEmpty && !dependant.isEmpty
      } yield "\"" + dependant + "\" -> \"" + dependsOn + "\""

    val lines =
      ("digraph " + graphName + " {") +:
        mappings :+
        "}"

    IO.writeLines(file, lines)
  }

  def sourceToString(roots: Iterable[File], source: File) =
    relativized(roots, source).trim.stripSuffix(".scala").stripSuffix(".java")

  private def relativized(roots: Iterable[File], path: File): String =
    {
      val relativized = roots.flatMap(root => IO.relativize(root, path))
      val shortest = (Int.MaxValue /: relativized)(_ min _.length)
      relativized.find(_.length == shortest).getOrElse(path.getName)
    }
}
