/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import java.io.{ File, FileNotFoundException, IOException }
import xsbti.{ FileRef, FileRefJarred, FileRefLoose }
import java.util.jar.JarFile
import scala.collection.JavaConverters._
import Stamp.getStamp
import scala.util.matching.Regex

trait ReadStamps {
  /** The Stamp for the given product at the time represented by this Stamps instance.*/
  def product(prod: FileRef): Stamp
  /** The Stamp for the given source file at the time represented by this Stamps instance.*/
  def internalSource(src: File): Stamp
  /** The Stamp for the given binary dependency at the time represented by this Stamps instance.*/
  def binary(bin: FileRef): Stamp
}

/** Provides information about files as they were at a specific time.*/
trait Stamps extends ReadStamps {
  def allInternalSources: collection.Set[File]
  def allBinaries: collection.Set[FileRef]
  def allProducts: collection.Set[FileRef]

  def sources: Map[File, Stamp]
  def binaries: Map[FileRef, Stamp]
  def products: Map[FileRef, Stamp]
  def classNames: Map[FileRef, String]

  def className(bin: FileRef): Option[String]

  def markInternalSource(src: File, s: Stamp): Stamps
  def markBinary(bin: FileRef, className: String, s: Stamp): Stamps
  def markProduct(prod: FileRef, s: Stamp): Stamps

  def filter(prod: FileRef => Boolean, removeSources: Iterable[File], bin: FileRef => Boolean): Stamps

  def ++(o: Stamps): Stamps
  def groupBy[K](prod: Map[K, FileRef => Boolean], sourcesGrouping: File => K, bin: Map[K, FileRef => Boolean]): Map[K, Stamps]
}

sealed trait Stamp {
  override def equals(other: Any): Boolean = other match {
    case o: Stamp => Stamp.equivStamp.equiv(this, o)
    case _        => false
  }

  override def toString: String = Stamp.toString(this)
}

final class Hash(val value: Array[Byte]) extends Stamp {
  override def hashCode: Int = java.util.Arrays.hashCode(value)
}
final class LastModified(val value: Long) extends Stamp {
  override def hashCode: Int = (value ^ (value >>> 32)).toInt
}
final class Exists(val value: Boolean) extends Stamp {
  override def hashCode: Int = if (value) 0 else 1
}

object Stamp {
  implicit val equivStamp: Equiv[Stamp] = new Equiv[Stamp] {
    def equiv(a: Stamp, b: Stamp) = (a, b) match {
      case (h1: Hash, h2: Hash)                   => h1.value sameElements h2.value
      case (e1: Exists, e2: Exists)               => e1.value == e2.value
      case (lm1: LastModified, lm2: LastModified) => lm1.value == lm2.value
      case _                                      => false
    }
  }

  // NOTE: toString/fromString used for serialization, not just for debug prints.

  def toString(s: Stamp): String = s match {
    case e: Exists        => if (e.value) "exists" else "absent"
    case h: Hash          => "hash(" + Hash.toHex(h.value) + ")"
    case lm: LastModified => "lastModified(" + lm.value + ")"
  }

  private val hashPattern = """hash\((\w+)\)""".r
  private val lastModifiedPattern = """lastModified\((\d+)\)""".r

  def fromString(s: String): Stamp = s match {
    case "exists"                   => new Exists(true)
    case "absent"                   => new Exists(false)
    case hashPattern(value)         => new Hash(Hash.fromHex(value))
    case lastModifiedPattern(value) => new LastModified(java.lang.Long.parseLong(value))
    case _                          => throw new IllegalArgumentException("Unrecognized Stamp string representation: " + s)
  }

  def show(s: Stamp): String = s match {
    case h: Hash          => "hash(" + Hash.toHex(h.value) + ")"
    case e: Exists        => if (e.value) "exists" else "does not exist"
    case lm: LastModified => "last modified(" + lm.value + ")"
  }

  val hash = (f: File) => tryStamp(new Hash(Hash(f)))
  val lastModifiedFile = (f: File) => tryStamp(new LastModified(f.lastModified))

  val exists = (f: File) => tryStamp(if (f.exists) present else notPresent)

  def tryStamp(g: => Stamp): Stamp = try { g } catch { case i: IOException => notPresent }

  val notPresent = new Exists(false)
  val present = new Exists(true)

  def getStamp[T](map: Map[T, Stamp], src: T): Stamp = map.getOrElse(src, notPresent)
}

object Stamps {
  /**
   * Creates a ReadStamps instance that will calculate and cache the stamp for sources and binaries
   * on the first request according to the provided `srcStamp` and `binStamp` functions.  Each
   * stamp is calculated separately on demand.
   * The stamp for a product is always recalculated.
   */
  def initial(): ReadStamps = new InitialStamps()

  val empty: Stamps = apply(Map.empty, Map.empty, Map.empty, Map.empty)

  def apply(products: Map[FileRef, Stamp], sources: Map[File, Stamp], binaries: Map[FileRef, Stamp], binaryClassNames: Map[FileRef, String]): Stamps =
    new MStamps(products, sources, binaries, binaryClassNames)

  def merge(stamps: Traversable[Stamps]): Stamps = (Stamps.empty /: stamps)(_ ++ _)
}

private class MStamps(val products: Map[FileRef, Stamp], val sources: Map[File, Stamp], val binaries: Map[FileRef, Stamp], val classNames: Map[FileRef, String]) extends Stamps {
  def allInternalSources: collection.Set[File] = sources.keySet
  def allBinaries: collection.Set[FileRef] = binaries.keySet
  def allProducts: collection.Set[FileRef] = products.keySet

  def ++(o: Stamps): Stamps =
    new MStamps(products ++ o.products, sources ++ o.sources, binaries ++ o.binaries, classNames ++ o.classNames)

  def markInternalSource(src: File, s: Stamp): Stamps =
    new MStamps(products, sources.updated(src, s), binaries, classNames)

  def markBinary(bin: FileRef, className: String, s: Stamp): Stamps =
    new MStamps(products, sources, binaries.updated(bin, s), classNames.updated(bin, className))

  def markProduct(prod: FileRef, s: Stamp): Stamps =
    new MStamps(products.updated(prod, s), sources, binaries, classNames)

  def filter(prod: FileRef => Boolean, removeSources: Iterable[File], bin: FileRef => Boolean): Stamps =
    new MStamps(products.filterKeys(prod), sources -- removeSources, binaries.filterKeys(bin), classNames.filterKeys(bin))

  def groupBy[K](prod: Map[K, FileRef => Boolean], f: File => K, bin: Map[K, FileRef => Boolean]): Map[K, Stamps] =
    {
      val sourcesMap: Map[K, Map[File, Stamp]] = sources.groupBy(x => f(x._1))

      def constFalse[F](f: F) = false
      def kStamps(k: K): Stamps = new MStamps(
        products.filterKeys(prod.getOrElse(k, constFalse)),
        sourcesMap.getOrElse(k, Map.empty[File, Stamp]),
        binaries.filterKeys(bin.getOrElse(k, constFalse)),
        classNames.filterKeys(bin.getOrElse(k, constFalse))
      )

      (for (k <- prod.keySet ++ sourcesMap.keySet ++ bin.keySet) yield (k, kStamps(k))).toMap
    }

  def product(prod: FileRef) = getStamp(products, prod)
  def internalSource(src: File) = getStamp(sources, src)
  def binary(bin: FileRef) = getStamp(binaries, bin)
  def className(bin: FileRef) = classNames get bin

  override def equals(other: Any): Boolean = other match {
    case o: MStamps => products == o.products && sources == o.sources && binaries == o.binaries && classNames == o.classNames
    case _          => false
  }

  override lazy val hashCode: Int = (products :: sources :: binaries :: classNames :: Nil).hashCode

  override def toString: String =
    "Stamps for: %d products, %d sources, %d binaries, %d classNames".format(products.size, sources.size, binaries.size, classNames.size)
}

/**
 * Stamps for inputs; once computed, these are cached and will never change.
 *
 * TODO: Check/enforce the assumption that InitialStamps will never be used in a position
 * where inputs have changed.
 *
 * In particular, this class caches Stamps for ALL files inside any jars
 * that are encountered.
 */
private class InitialStamps extends ReadStamps {
  import collection.mutable
  // cached stamps for files
  private val files = mutable.Map[File, Stamp]()
  // map from jar path to map of internal classfile to Stamp
  private val jars = mutable.Map[File, Map[String, Stamp]]()

  def product(prod: FileRef): Stamp = lastModified(prod)
  def internalSource(src: File): Stamp = lastModified(src)
  def binary(bin: FileRef): Stamp = lastModified(bin)

  private def lastModified(f: File): Stamp = files.synchronized {
    files.getOrElseUpdate(f, Stamp.lastModifiedFile(f))
  }

  private def lastModified(u: FileRef): Stamp = Stamp.tryStamp {
    u match {
      case cr: FileRefLoose =>
        lastModified(cr.classFile)
      case cr: FileRefJarred =>
        lastModified(cr.jarFile, cr.classFile)
    }
  }

  private def lastModified(jarFile: File, classFile: String): Stamp =
    jars.synchronized {
      jars.getOrElseUpdate(jarFile, {
        new JarFile(jarFile).entries.asScala.map { jarEntry =>
          jarEntry.getName -> new LastModified(jarEntry.getTime)
        }.toMap
      })
    }.getOrElse(classFile, {
      throw new FileNotFoundException(s"Could not locate ${classFile} in ${jarFile}")
    })
}
