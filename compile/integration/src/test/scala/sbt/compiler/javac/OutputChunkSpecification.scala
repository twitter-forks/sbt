package sbt.compiler.javac

import java.io.File
import collection.JavaConverters._

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import sbt.{ IO, Logger, Using }
import xsbti.compile.SingleOutput
import xsbti.{ ClassRef, ClassRefLoose, ClassRefJarred }

@RunWith(classOf[JUnitRunner])
class OutputChunkSpecification extends Specification {

  "OutputChunk" should {
    "capture outputs for Jars" in {
      IO.withTemporaryDirectory { dir =>
        val outputFile = new File(dir, "blah.jar")
        val c = OutputChunk(new SingleOutput { def outputLocation = outputFile }, Seq())
        val l = Logger.Null

        // create a class and confirm that it is in the only one in the output
        val refA = new ClassRefJarred(outputFile, "a.class")
        captureAdd(c, refA.classFile) === Set(refA)

        // create another class and confirm that both are in the output
        val refB = new ClassRefJarred(outputFile, "b.class")
        captureAdd(c, refB.classFile) === Set(refA, refB)
      }
    }

    /**
     * Captures creating the given filename, and asserts that the capture is recorded. Returns
     * the new content of the OutputChunk, according to the chunk.
     */
    def captureAdd(c: OutputChunk, filename: String): Set[ClassRef] = {
      c.capture { tempOutput =>
        IO.touch(new File(tempOutput.outputLocation, filename))
      }.toSet === Set(new ClassRefJarred(c.outputFile, filename))

      c.getCurrentRefs().toSet
    }
  }
}
