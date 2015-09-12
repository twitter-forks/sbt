package sbt.compiler.javac

import java.io.{ File, IOException }

import sbt._
import sbt.classfile.Analyze
import sbt.classpath.ClasspathUtilities
import sbt.compiler.CompilerArguments
import sbt.inc.Locate
import xsbti.api.Source
import xsbti.compile._
import xsbti.{ AnalysisCallback, ClassRef, Reporter }

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
        chunk.capture(log) { tmpOutput =>
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
