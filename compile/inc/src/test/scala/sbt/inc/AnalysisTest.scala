package sbt
package inc

import java.io.File
import java.net.URL
import scala.math.abs
import sbt.inc.TestCaseGenerators._
import org.scalacheck._
import Gen._
import Prop._
import xsbti.DependencyContext._
import xsbti.{ FileRef, FileRefJarred, FileRefLoose }

object AnalysisTest extends Properties("Analysis") {
  // Merge and split a hard-coded trivial example.
  property("Simple Merge and Split") = {
    val aScala = f("A.scala")
    val bScala = f("B.scala")
    val xClass = u("x.jar", "x.class")
    val yClass = u("y.jar", "y.class")
    val aSource = genSource("A" :: "A$" :: Nil).sample.get
    val bSource = genSource("B" :: "B$" :: Nil).sample.get
    val cSource = genSource("C" :: Nil).sample.get
    val exists = new Exists(true)
    val sourceInfos = SourceInfos.makeInfo(Nil, Nil)

    // a
    val aProducts = (u("A.class"), "A", exists) :: (u("A$.class"), "A$", exists) :: Nil
    val aInternal = Nil
    val aExternal = ExternalDependency(aScala, "C", cSource, DependencyByMemberRef) :: Nil
    val aBinary = (xClass, "x", exists) :: Nil

    val a = Analysis.empty(false).addSource(aScala, aSource, exists, sourceInfos, aProducts, aInternal, aExternal, aBinary)

    // b
    val bProducts = (u("B.class"), "B", exists) :: (u("B$.class"), "B$", exists) :: Nil
    val bInternal = Nil
    val bExternal = ExternalDependency(bScala, "A", aSource, DependencyByInheritance) :: Nil
    val bBinary = (xClass, "x", exists) :: (yClass, "y", exists) :: Nil

    val b = Analysis.empty(false).addSource(bScala, bSource, exists, sourceInfos, bProducts, bInternal, bExternal, bBinary)

    // ab
    // `b` has an external dependency on `a` that will be internalized
    val abAProducts = (u("A.class"), "A", exists) :: (u("A$.class"), "A$", exists) :: Nil
    val abAInternal = Nil
    val abAExternal = ExternalDependency(aScala, "C", cSource, DependencyByMemberRef) :: Nil
    val abABinary = (xClass, "x", exists) :: Nil

    val abBProducts = (u("B.class"), "B", exists) :: (u("B$.class"), "B$", exists) :: Nil
    val abBInternal = InternalDependency(bScala, aScala, DependencyByMemberRef) :: InternalDependency(bScala, aScala, DependencyByInheritance) :: Nil
    val abBExternal = Nil
    val abBBinary = (xClass, "x", exists) :: (yClass, "y", exists) :: Nil

    val ab = Analysis.empty(false).addSource(aScala, aSource, exists, sourceInfos, abAProducts, abAInternal, abAExternal, abABinary)
      .addSource(bScala, bSource, exists, sourceInfos, abBProducts, abBInternal, abBExternal, abBBinary)

    val split: Map[String, Analysis] = ab.groupBy({ f: File => f.getName.substring(0, 1) })

    val aSplit = split.getOrElse("A", Analysis.empty(false))
    val bSplit = split.getOrElse("B", Analysis.empty(false))

    val merged = Analysis.merge(a :: b :: Nil)

    ("split(AB)(A) == A" |: compare(a, aSplit)) &&
      ("split(AB)(B) == B" |: compare(b, bSplit)) &&
      ("merge(A, B) == AB" |: compare(merged, ab))
  }

  // Merge and split large, generated examples.
  // Mustn't shrink, as the default Shrink[Int] doesn't respect the lower bound of choose(), which will cause
  // a divide-by-zero error masking the original error.
  // Note that the generated Analyses have nameHashing = false (Grouping of Analyses with name hashing enabled
  // is not supported right now)
  property("Complex Merge and Split") = forAllNoShrink(genAnalysis(nameHashing = false), choose(1, 10)) { (analysis: Analysis, numSplits: Int) =>
    val grouped: Map[Int, Analysis] = analysis.groupBy({ f: File => abs(f.hashCode()) % numSplits })
    def getGroup(i: Int): Analysis = grouped.getOrElse(i, Analysis.empty(false))
    val splits = (Range(0, numSplits) map getGroup).toList

    val merged: Analysis = Analysis.merge(splits)
    "Merge all" |: compare(analysis, merged)
  }

  // Compare two analyses with useful labelling when they aren't equal.
  private[this] def compare(left: Analysis, right: Analysis): Prop =
    s" LEFT: $left" |:
      s"RIGHT: $right" |:
      s"STAMPS EQUAL: ${left.stamps == right.stamps}" |:
      s"APIS EQUAL: ${left.apis == right.apis}" |:
      s"RELATIONS EQUAL: ${left.relations == right.relations}" |:
      "UNEQUAL" |:
      (left == right)

  def f(s: String) = new File(s)
  def u(cls: String) = new FileRefLoose(f(cls))
  def u(jar: String, cls: String) = new FileRefJarred(f(jar), cls)
}
