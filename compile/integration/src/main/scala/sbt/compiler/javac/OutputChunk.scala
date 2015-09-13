package sbt.compiler.javac

import java.io.{ File, IOException }
import collection.JavaConverters._

import sbt.{ IO, PathFinder, Using }
import xsbti.compile.SingleOutput
import xsbti.{ ClassRef, ClassRefLoose, ClassRefJarred }

sealed trait OutputChunk {
  /** The source files owned by the chunk */
  def sources: Seq[File]
  /** The final Output location of this chunk. */
  def output: SingleOutput
  /** The File for the output location of this chunk. */
  def outputFile: File = output.outputLocation
  /**
   * Executes the given function in the context of a compilation, and returns the classfiles
   * that were created.
   */
  def capture(f: SingleOutput => Unit): Seq[ClassRef]
}

object OutputChunk {
  /** Capture the current outputs in the given location. */
  def apply(output: SingleOutput, sources: Seq[File]): OutputChunk =
    if (output.outputLocation.getName.endsWith(".jar")) {
      Jar(output, sources)
    } else {
      Directory(output, sources)
    }

  private def listClasses(directory: File): Seq[File] = (PathFinder(directory) ** "*.class").get

  case class Directory private[OutputChunk] (output: SingleOutput, sources: Seq[File]) extends OutputChunk {
    def getCurrentClasses(): Seq[File] = listClasses(outputFile)
    def capture(f: SingleOutput => Unit) = {
      val preClasses = getCurrentClasses()
      f(output)
      (preClasses.toSet -- getCurrentClasses()).toSeq.map(new ClassRefLoose(_))
    }
  }
  case class Jar private[OutputChunk] (output: SingleOutput, sources: Seq[File]) extends OutputChunk {
    def getCurrentClasses(): Set[String] =
      if (outputFile.exists) {
        Using.jarFile(false)(outputFile) { jf =>
          jf.entries.asScala.map(_.getName).toSet
        }
      } else {
        Set()
      }
    def capture(f: SingleOutput => Unit) =
      IO.withTemporaryDirectory(output.outputLocation.getParentFile) { tempDir =>
        // capture current files in the jar
        val preClasses = getCurrentClasses()

        // then execute the operation and capture new files
        f(new SingleOutput { def outputLocation = tempDir })
        lazy val now = System.currentTimeMillis
        val newClasses = listClasses(tempDir)
        val newEntries =
          newClasses.map { newClass =>
            IO.zipEntry(tempDir, newClass, now).getOrElse {
              throw new IOException(s"Output class $newClass not located under expected directory $tempDir.")
            }
          }
        val newEntryNames = newEntries.map(_.getName).toSet

        // create a temporary jar and add all relevant classes to it
        val tempJar = new File(outputFile.getParent, outputFile.getName + ".tmp")
        Using.fileOutputStream(append = false)(tempJar) { tfStream =>
          Using.jarOutputStream(tfStream) { tjfStream =>
            // add new classes to the jar
            (newEntries, newClasses).zipped.foreach { (newEntry, newClass) =>
              tjfStream.putNextEntry(newEntry)
              IO.transfer(newClass, tjfStream)
              tjfStream.closeEntry()
            }
            // then copy surviving classes from the previous jar into the new jar
            if (outputFile.exists) {
              Using.fileInputStream(outputFile) { ifStream =>
                Using.jarInputStream(ifStream) { jifStream =>
                  IO.transfer(jifStream, tjfStream, je => !newEntryNames.contains(je.getName))
                }
              }
            }
          }
        }
        // move the temporary jar to its final location
        IO.move(tempJar, outputFile)

        // and return ClassRefs for newly added classes
        (newEntryNames -- preClasses).toSeq.map(new ClassRefJarred(outputFile, _))
      }
  }
}
