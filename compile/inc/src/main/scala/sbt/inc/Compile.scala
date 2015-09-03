/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package inc

import xsbti.api.{ Source, SourceAPI, Compilation, OutputSetting, _internalOnly_NameHashes }
import xsbti.compile.{ DependencyChanges, Output, SingleOutput, MultipleOutput }
import xsbti.{ Position, Problem, Severity }
import Logger.{ m2o, problem }
import java.io.File
import java.net.URL
import xsbti.api.Definition
import xsbti.DependencyContext
import xsbti.DependencyContext.{ DependencyByInheritance, DependencyByMemberRef }

/**
 * Helper methods for running incremental compilation.  All this is responsible for is
 * adapting any xsbti.AnalysisCallback into one compatible with the [[sbt.inc.Incremental]] class.
 */
object IncrementalCompile {
  /**
   * Runs the incremental compilation algorithm.
   * @param sources
   *              The full set of input sources
   * @param entry
   *              A className -> source file lookup function.
   * @param compile
   *                The mechanism to run a single 'step' of compile, for ALL source files involved.
   * @param previous
   *                 The previous dependency Analysis (or an empty one).
   * @param forEntry
   *                 The dependency Analysis associated with a given file
   * @param output
   *               The configured output directory/directory mapping for source files.
   * @param log
   *            Where all log messages should go
   * @param options
   *                Incremental compiler options (like name hashing vs. not).
   * @return
   *         A flag of whether or not compilation completed succesfully, and the resulting dependency analysis object.
   *
   */
  def apply(sources: Set[File], entry: String => Option[ClassRef],
    compile: (Set[File], DependencyChanges, xsbti.AnalysisCallback) => Unit,
    previous: Analysis,
    forEntry: File => Option[Analysis],
    output: Output, log: Logger,
    options: IncOptions): (Boolean, Analysis) =
    {
      val current = Stamps.initial()
      val internalMap = (f: ClassRef) => previous.relations.produced(f).headOption
      val externalAPI = getExternalAPI(entry, forEntry)
      try {
        Incremental.compile(sources, entry, previous, current, forEntry, doCompile(compile, internalMap, externalAPI, current, output, options), log, options)
      } catch {
        case e: xsbti.CompileCancelled =>
          log.info("Compilation has been cancelled")
          // in case compilation got cancelled potential partial compilation results (e.g. produced classs files) got rolled back
          // and we can report back as there was no change (false) and return a previous Analysis which is still up-to-date
          (false, previous)
      }
    }
  def doCompile(compile: (Set[File], DependencyChanges, xsbti.AnalysisCallback) => Unit, internalMap: ClassRef => Option[File], externalAPI: (ClassRef, String) => Option[Source], current: ReadStamps, output: Output, options: IncOptions) =
    (srcs: Set[File], changes: DependencyChanges) => {
      val callback = new AnalysisCallback(internalMap, externalAPI, current, output, options)
      compile(srcs, changes, callback)
      callback.get
    }
  def getExternalAPI(entry: String => Option[ClassRef], forEntry: File => Option[Analysis]): (ClassRef, String) => Option[Source] =
    (file: ClassRef, className: String) =>
      entry(className) flatMap { defines =>
        forEntry(IO.asFile(defines)) flatMap { analysis =>
          analysis.relations.definesClass(className).headOption flatMap { src =>
            analysis.apis.internal get src
          }
        }
      }
}
private final class AnalysisCallback(internalMap: ClassRef => Option[File], externalAPI: (ClassRef, String) => Option[Source], current: ReadStamps, output: Output, options: IncOptions) extends xsbti.AnalysisCallback {
  val compilation = {
    val outputSettings = output match {
      case single: SingleOutput => Array(new OutputSetting("/", single.outputLocation.getAbsolutePath))
      case multi: MultipleOutput =>
        multi.outputGroups.map(out => new OutputSetting(out.sourceDirectory.getAbsolutePath, out.outputDirectory.getAbsolutePath))
    }
    new Compilation(System.currentTimeMillis, outputSettings)
  }

  override def toString = (List("APIs", "Binary deps", "Products", "Source deps") zip List(apis, binaryDeps, classes, intSrcDeps)).map { case (label, map) => label + "\n\t" + map.mkString("\n\t") }.mkString("\n")

  import collection.mutable.{ HashMap, HashSet, ListBuffer, Map, Set }

  private[this] val apis = new HashMap[File, (Int, SourceAPI)]
  private[this] val usedNames = new HashMap[File, Set[String]]
  private[this] val publicNameHashes = new HashMap[File, _internalOnly_NameHashes]
  private[this] val unreporteds = new HashMap[File, ListBuffer[Problem]]
  private[this] val reporteds = new HashMap[File, ListBuffer[Problem]]
  private[this] val binaryDeps = new HashMap[File, Set[ClassRef]]
  // source file to set of generated (class file, class name)
  private[this] val classes = new HashMap[File, Set[(ClassRef, String)]]
  // generated class file to its source file
  private[this] val classToSource = new HashMap[ClassRef, File]
  // internal source dependencies
  private[this] val intSrcDeps = new HashMap[File, Set[InternalDependency]]
  // external source dependencies
  private[this] val extSrcDeps = new HashMap[File, Set[ExternalDependency]]
  private[this] val binaryClassName = new HashMap[ClassRef, String]
  // source files containing a macro def.
  private[this] val macroSources = Set[File]()

  private def add[A, B](map: Map[A, Set[B]], a: A, b: B): Unit =
    map.getOrElseUpdate(a, new HashSet[B]) += b

  def problem(category: String, pos: Position, msg: String, severity: Severity, reported: Boolean): Unit =
    {
      for (source <- m2o(pos.sourceFile)) {
        val map = if (reported) reporteds else unreporteds
        map.getOrElseUpdate(source, ListBuffer.empty) += Logger.problem(category, pos, msg, severity)
      }
    }

  def sourceDependency(dependsOn: File, source: File, context: DependencyContext) = {
    add(intSrcDeps, source, InternalDependency(source, dependsOn, context))
  }

  @deprecated("Use `sourceDependency(File, File, DependencyContext)`.", "0.13.8")
  def sourceDependency(dependsOn: File, source: File, inherited: Boolean) =
    {
      val context = if (inherited) DependencyByInheritance else DependencyByMemberRef
      sourceDependency(dependsOn, source, context)
    }

  private[this] def externalBinaryDependency(binary: ClassRef, className: String, source: File, context: DependencyContext) = {
    binaryClassName.put(binary, className)
    add(binaryDeps, source, binary)
  }

  private[this] def externalSourceDependency(sourceFile: File, dependsOn: String, source: Source, context: DependencyContext) = {
    val dependency = ExternalDependency(sourceFile, dependsOn, source, context)
    add(extSrcDeps, sourceFile, dependency)
  }

  def binaryDependency(classFile: ClassRef, name: String, source: File, context: DependencyContext) =
    internalMap(classFile) match {
      case Some(dependsOn) =>
        // dependency is a product of a source not included in this compilation
        sourceDependency(dependsOn, source, context)
      case None =>
        classToSource.get(classFile) match {
          case Some(dependsOn) =>
            // dependency is a product of a source in this compilation step,
            //  but not in the same compiler run (as in javac v. scalac)
            sourceDependency(dependsOn, source, context)
          case None =>
            externalDependency(classFile, name, source, context)
        }
    }

  private[this] def externalDependency(classFile: ClassRef, name: String, source: File, context: DependencyContext): Unit =
    externalAPI(classFile, name) match {
      case Some(api) =>
        // dependency is a product of a source in another project
        externalSourceDependency(source, name, api, context)
      case None =>
        // dependency is some other binary on the classpath
        externalBinaryDependency(classFile, name, source, context)
    }

  def generatedClass(source: File, module: ClassRef, name: String) = {
    add(classes, source, (module, name))
    classToSource.put(module, source)
  }

  // empty value used when name hashing algorithm is disabled
  private val emptyNameHashes = new xsbti.api._internalOnly_NameHashes(Array.empty, Array.empty)

  def api(sourceFile: File, source: SourceAPI): Unit = {
    import xsbt.api.{ APIUtil, HashAPI }
    if (APIUtil.isScalaSourceName(sourceFile.getName) && APIUtil.hasMacro(source)) macroSources += sourceFile
    publicNameHashes(sourceFile) = {
      if (nameHashing)
        (new xsbt.api.NameHashing).nameHashes(source)
      else
        emptyNameHashes
    }
    val shouldMinimize = !Incremental.apiDebug(options)
    val savedSource = if (shouldMinimize) APIUtil.minimize(source) else source
    apis(sourceFile) = (HashAPI(source), savedSource)
  }

  def usedName(sourceFile: File, name: String) = add(usedNames, sourceFile, name)

  def nameHashing: Boolean = options.nameHashing

  def get: Analysis = addUsedNames(addCompilation(addProductsAndDeps(Analysis.empty(nameHashing = nameHashing))))

  def getOrNil[A, B](m: collection.Map[A, Seq[B]], a: A): Seq[B] = m.get(a).toList.flatten
  def addCompilation(base: Analysis): Analysis = base.copy(compilations = base.compilations.add(compilation))
  def addUsedNames(base: Analysis): Analysis = (base /: usedNames) {
    case (a, (src, names)) =>
      (a /: names) { case (a, name) => a.copy(relations = a.relations.addUsedName(src, name)) }
  }

  def addProductsAndDeps(base: Analysis): Analysis =
    (base /: apis) {
      case (a, (src, api)) =>
        val stamp = current.internalSource(src)
        val hash = stamp match { case h: Hash => h.value; case _ => new Array[Byte](0) }
        // TODO store this in Relations, rather than Source.
        val hasMacro: Boolean = macroSources.contains(src)
        val s = new xsbti.api.Source(compilation, hash, api._2, api._1, publicNameHashes(src), hasMacro)
        val info = SourceInfos.makeInfo(getOrNil(reporteds, src), getOrNil(unreporteds, src))
        val binaries: Iterable[ClassRef] = binaryDeps.getOrElse(src, Nil)
        val prods: Iterable[(ClassRef, String)] = classes.getOrElse(src, Nil)

        val products = prods.map { case (prod, name) => (prod, name, current product prod) }
        val internalDeps = intSrcDeps.getOrElse(src, Set.empty)
        val externalDeps = extSrcDeps.getOrElse(src, Set.empty)
        val binDeps = binaries.map(d => (d, binaryClassName(d), current binary d))

        a.addSource(src, s, stamp, info, products, internalDeps, externalDeps, binDeps)

    }
}
