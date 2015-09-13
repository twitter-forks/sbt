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

        // create a class and confirm that the newly added class is captured
        val fileNameA = "a.class"
        c.capture { tempOutput =>
          IO.touch(new File(tempOutput.outputLocation, fileNameA))
        }.toSet === Set(new ClassRefJarred(outputFile, fileNameA))

        // create another class
        val fileNameB = "b.class"
        c.capture { tempOutput =>
          IO.touch(new File(tempOutput.outputLocation, fileNameB))
        }.toSet === Set(new ClassRefJarred(outputFile, fileNameB))

        // confirm that the output jar contains both classes
        Using.jarFile(false)(outputFile) { jf =>
          jf.entries.asScala.map(_.getName).toSet
        } === Set(fileNameA, fileNameB)
      }
    }
  }
}
