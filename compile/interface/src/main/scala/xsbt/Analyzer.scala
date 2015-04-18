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
final class Analyzer(val global: CallbackGlobal) {
  import global._

  def newPhase(prev: Phase): Phase = new AnalyzerPhase(prev)
  private class AnalyzerPhase(prev: Phase) extends Phase(prev) {
    override def description = "Finds concrete instances of provided superclasses, and application entry points."

    def name = Analyzer.name
    def run {
      lazy val locator = ClassFileLocator(global)

      // build list of generated classes
      for (unit <- currentRun.units if !unit.isJava) {
        val sourceFile = unit.source.file.file
        for (iclass <- unit.icode) {
          val sym = iclass.symbol
          def addGenerated(separatorRequired: Boolean) {
            val classFile = locator.getOutputClass(sym, separatorRequired)
            val className = locator.className(sym, '.', separatorRequired)
            callback.generatedClass(sourceFile, classFile, className)
          }
          if (sym.isModuleClass && !sym.isImplClass) {
            if (locator.isTopLevelModule(sym) && sym.companionClass == NoSymbol)
              addGenerated(false)
            addGenerated(true)
          } else
            addGenerated(false)
        }
      }
    }
  }
}
