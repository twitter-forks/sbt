package sbt.compiler.javac

import java.io.File
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
    val outputLocation: File
    /** The source files owned by the chunk */
    val sources: Seq[File]
    /** The classes at the output location at construction time. */
    val oldClasses: Seq[ClassRef] = getCurrentClasses()
    /** Computes the _current_ classes at this output location. */
    def getCurrentClasses(): Seq[ClassRef]
  }

  private object OutputChunk {
    /** Capture the current outputs in the given location. */
    def apply(outputLocation: File, sources: Seq[File]): OutputChunk =
      if (outputLocation.isFile && outputLocation.getName.endsWith(".jar")) {
        Jar(outputLocation, sources)
      } else {
        Directory(outputLocation, sources)
      }

    case class Directory(outputLocation: File, sources: Seq[File]) extends OutputChunk {
      def getCurrentClasses(): Seq[ClassRef] =
        (PathFinder(outputLocation) ** "*.class").get.map(new ClassRefLoose(_))
    }
    case class Jar(outputLocation: File, sources: Seq[File]) extends OutputChunk {
      def getCurrentClasses(): Seq[ClassRef] =
        Using.jarFile(false)(outputLocation) { jf =>
          jf.entries.asScala.map { je =>
            new ClassRefJarred(outputLocation, je.getName)
          }.toSeq
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

      // Here we outline "chunks" of compiles we need to run so that the .class files end up in the right
      // location for Java. This also captures the known classfiles in the output directory to determine
      // which classfiles were added by the compile.
      val chunks =
        output match {
          case single: SingleOutput =>
            Seq(OutputChunk(single.outputLocation, sources))
          case multi: MultipleOutput =>
            sources.groupBy { src =>
              multi.outputGroups find { out => ancestor(out.sourceDirectory, src) } map (_.outputDirectory)
            }.flatMap {
              case (None, srcs) =>
                // Report warnings about source files that have no output directory.
                // TODO: silently dropping analysis is odd...
                log.error("No output directory mapped for: " + srcs.map(_.getAbsolutePath).mkString(","))
                None
              case (Some(outputLocation), srcs) =>
                Some(OutputChunk(outputLocation, srcs))
            }
        }
      // Construct a class-loader we'll use to find dependencies
      val loader = ClasspathUtilities.toLoader(searchClasspath)

      // Execute the compilation
      // TODO - Perhaps we just record task 1 here
      try javac.compileWithReporter(sources.toArray, absClasspath.toArray, output, options.toArray, reporter, log)
      catch {
        // Handle older APIs
        case _: NoSuchMethodError =>
          javac.compile(sources.toArray, absClasspath.toArray, output, options.toArray, log)
      }
      // TODO - Perhaps we just record task 2 here

      // Runs the analysis portion of Javac.
      timed("Java analysis", log) {
        // Reads the API information directly from the Class[_] object
        def readAPI(source: File, classes: Seq[Class[_]]): Set[String] = {
          val (api, inherits) = ClassToAPI.process(classes)
          callback.api(source, api)
          inherits.map(_.getName)
        }

        // Analyze each chunk by comparing old and new classes
        chunks.foreach { outputChunk =>
          Analyze(outputChunk.getCurrentClasses().toSeq, outputChunk.sources, log)(callback, loader, readAPI)
        }
      }
      // TODO - Perhaps we just record task 3 here
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
