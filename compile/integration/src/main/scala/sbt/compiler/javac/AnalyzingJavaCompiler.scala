package sbt.compiler.javac

import java.io.{ File, IOException }
import collection.JavaConverters._

import sbt._
import sbt.classfile.Analyze
import sbt.classpath.ClasspathUtilities
import sbt.compiler.CompilerArguments
import sbt.inc.Locate
import xsbti.api.Source
import xsbti.compile._
import xsbti.{ AnalysisCallback, ClassRef, ClassRefLoose, ClassRefJarred, Reporter }

/**
 * This is a java compiler which will also report any discovered source dependencies/apis out via
 * an analysis callback.
 *
 * @param searchClasspath Differes from classpath in that we look up binary dependencies via this classpath.
 * @param classLookup A mechanism by which we can figure out whether any classpath entry contains a classfile.
 */
final class AnalyzingJavaCompiler private[sbt] (
    val javac: xsbti.compile.JavaCompiler,
    val classpath: Seq[File],
    val scalaInstance: xsbti.compile.ScalaInstance,
    val classLookup: (String => Option[ClassRef]),
    val searchClasspath: Seq[File]) {

  sealed private trait OutputChunk {
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

  private object OutputChunk {
    /** Capture the current outputs in the given location. */
    def apply(output: SingleOutput, sources: Seq[File]): OutputChunk =
      if (output.outputLocation.getName.endsWith(".jar")) {
        Jar(output, sources)
      } else {
        Directory(output, sources)
      }

    private def listClasses(directory: File): Seq[File] = (PathFinder(directory) ** "*.class").get

    case class Directory(output: SingleOutput, sources: Seq[File]) extends OutputChunk {
      def getCurrentClasses(): Seq[File] = listClasses(outputFile)
      def capture(f: SingleOutput => Unit) = {
        val preClasses = getCurrentClasses()
        f(output)
        (preClasses.toSet -- getCurrentClasses()).toSeq.map(new ClassRefLoose(_))
      }
    }
    case class Jar(output: SingleOutput, sources: Seq[File]) extends OutputChunk {
      def getCurrentClasses(): Seq[String] =
        Using.jarFile(false)(outputFile) { jf =>
          jf.entries.asScala.map(_.getName).toSeq
        }
      def capture(f: SingleOutput => Unit): Seq[ClassRef] =
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
              Using.fileInputStream(outputFile) { ifStream =>
                Using.jarInputStream(ifStream) { jifStream =>
                  IO.transfer(jifStream, tjfStream, je => !newEntryNames.contains(je.getName))
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

  /**
   * Compile some java code using the current configured compiler.
   *
   * @param sources  The sources to compile
   * @param options  The options for the Java compiler
   * @param output   The output configuration for this compiler
   * @param callback  A callback to report discovered source/binary dependencies on.
   * @param reporter  A reporter where semantic compiler failures can be reported.
   * @param log       A place where we can log debugging/error messages.
   * @param progressOpt An optional compilation progress reporter.  Where we can report back what files we're currently compiling.
   */
  def compile(sources: Seq[File], options: Seq[String], output: Output, callback: AnalysisCallback, reporter: Reporter, log: Logger, progressOpt: Option[CompileProgress]): Unit = {
    if (sources.nonEmpty) {
      val absClasspath = classpath.map(_.getAbsoluteFile)
      @annotation.tailrec def ancestor(f1: File, f2: File): Boolean =
        if (f2 eq null) false else if (f1 == f2) true else ancestor(f1, f2.getParentFile)

      // TODO - Perhaps we just record task 0 here

      // Capture the known classfiles in the output directory to determine which classfiles were added by the compile.
      val chunk =
        output match {
          case single: SingleOutput =>
            OutputChunk(single, sources)
          case multi: MultipleOutput =>
            throw new RuntimeException(s"Javac doesn't support multiple output directories: $multi")
        }
      // Construct a class-loader we'll use to find dependencies
      val loader = ClasspathUtilities.toLoader(searchClasspath)

      // Execute the compilation
      // TODO - Perhaps we just record task 1 here
      val newClasses =
        chunk.capture { tmpOutput =>
          try javac.compileWithReporter(sources.toArray, absClasspath.toArray, tmpOutput, options.toArray, reporter, log)
          catch {
            // Handle older APIs
            case _: NoSuchMethodError =>
              javac.compile(sources.toArray, absClasspath.toArray, tmpOutput, options.toArray, log)
          }
        }
      // TODO - Perhaps we just record task 2 here

      // Runs the analysis portion of Javac.
      // TODO - Perhaps we just record task 3 here
      timed("Java analysis", log) {
        // Reads the API information directly from the Class[_] object
        def readAPI(source: File, classes: Seq[Class[_]]): Set[String] = {
          val (api, inherits) = ClassToAPI.process(classes)
          callback.api(source, api)
          inherits.map(_.getName)
        }

        // Analyze each chunk by comparing old and new classes
        Analyze(newClasses, chunk.sources, log)(callback, loader, readAPI)
      }
      // TODO - Perhaps we just record task 4 here
    }
  }

  /** Debugging method to time how long it takes to run various compilation tasks. */
  private[this] def timed[T](label: String, log: Logger)(t: => T): T = {
    val start = System.nanoTime
    val result = t
    val elapsed = System.nanoTime - start
    log.debug(label + " took " + (elapsed / 1e9) + " s")
    result
  }
}
