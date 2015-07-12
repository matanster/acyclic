package extractor.plugin
import scala.collection.{SortedSet, mutable}
import scala.tools.nsc.{Global, Phase}
import tools.nsc.plugins.PluginComponent

class PluginPhase(val global: Global)
                  extends PluginComponent
                  { t =>

  import global._

  val runsAfter = List("typer")

  override val runsRightAfter = Some("typer")

  val phaseName = "grapher"
  def pkgName(unit: CompilationUnit) = { // typically a unit is a source file
                                         // println("\nSource file: " + unit) // print source file name
    unit.body
        .collect{case x: PackageDef => x.pid.toString}
        .flatMap(_.split('.'))
  }

  def units = global.currentRun
                    .units
                    .toSeq
                    .sortBy(_.source.content.mkString.hashCode())

  override def newPhase(prev: Phase): Phase = new Phase(prev) {
    override def run() {
      val unitMap = units.map(u => u.source.path -> u).toMap
      val nodes = for (unit <- units) yield {

        //println(unit.source.path)
        //println("----------------")
        
        // get the tree for each symbol
        DependencyExtraction(t.global)(unit)  
      }

    }

    def name: String = "acyclic"
  }

}
