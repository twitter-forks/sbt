/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.tools.nsc.{ io, plugins, symtab, Global, Phase }
import io.{ AbstractFile, PlainFile, ZipArchive }
import plugins.{ Plugin, PluginComponent }

import java.io.File
import xsbti.{ AnalysisCallback, DirectoryOutputLocation, JarOutputLocation, OutputLocation }

object Analyzer {
  def name = "xsbt-analyzer"
}
final class Analyzer(val global: CallbackGlobal) extends LocateClassFile {
  import global._

  def newPhase(prev: Phase): Phase = new AnalyzerPhase(prev)
  private class AnalyzerPhase(prev: Phase) extends Phase(prev) {
    override def description = "Finds concrete instances of provided superclasses, and application entry points."

    def name = Analyzer.name
    def run {
      lazy val locator = classFileLocator()

      // build list of generated classes
      for (unit <- currentRun.units if !unit.isJava) {
        val sourceFile = unit.source.file.file
        for (iclass <- unit.icode) {
          val sym = iclass.symbol
          def addGenerated(separatorRequired: Boolean): Unit = {
            locator.getOutputClass(sym, separatorRequired).foreach { classRef =>
              callback.generatedClass(sourceFile, classRef, className(sym, '.', separatorRequired))
            }
          }
          if (sym.isModuleClass && !sym.isImplClass) {
            if (isTopLevelModule(sym) && sym.companionClass == NoSymbol)
              addGenerated(false)
            addGenerated(true)
          } else
            addGenerated(false)
        }
      }
    }
  }
}
