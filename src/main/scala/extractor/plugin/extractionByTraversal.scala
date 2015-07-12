package extractor.plugin
import scala.tools.nsc.Global
object DependencyExtraction{
  
  def apply(global: Global)(unit: global.CompilationUnit) = {
    import global._

    class ExtractAll(defParent: Option[global.Symbol]) extends Traverser {
      override def traverse(tree: Tree): Unit = {
        
        (defParent.isDefined) match {
          case true => println("---- entering traverser with parent value passed " + defParent.get.id  + " ----")
          case false => println("==== entering traverser without parent value been passed ====")
        }
        
        tree match {
          case i @ Import(expr, selectors) =>
            selectors.foreach {
              case ImportSelector(nme.WILDCARD, _, null, _) => // a wildcard import
                // in case of wildcard import we do not rely on any particular name being defined
                // on `expr`; all symbols that are being used will get caught through selections
                //println(tree)
              case ImportSelector(name: Name, _, _, _) => // a specific import
                def lookupImported(name: Name) = expr.symbol.info.member(name)
                // importing a name means importing both a term and a type (if they exist)
                //println(tree); println(lookupImported(name) + "(" + lookupImported(name).id + ")")
            }
          case select: Select =>
            ////println(select.tpe + " contains symbols: ")
            select.symbol.kindString match {
              case "constructor" => // ignore
              case "method" =>      println(defParent.getOrElse("root") + " uses: " + select.symbol + " (" + select.symbol.id + ")" +  " of " + select.symbol.owner + " owned by " + select.symbol.owner.owner)
              case _ =>             println(defParent.getOrElse("root") + " uses: " + select.symbol + " of type " + select.symbol.tpe.typeSymbol)
            }
            
            //select.symbol.tpe.typeSymbol + " or rather of " + select.symbol.owner)
            
          /*
           * Idents are used in number of situations:
           *  - to refer to local variable
           *  - to refer to a top-level package (other packages are nested selections)
           *  - to refer to a term defined in the same package as an enclosing class;
           *    this looks fishy, see this thread:
           *    https://groups.google.com/d/topic/scala-internals/Ms9WUAtokLo/discussion
           */
          case ident: Ident =>
            //println(ident.symbol)
          case typeTree: TypeTree  => // are we missing something by not handling this?
          
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            val s = tree.symbol
            println(defParent.get.id + " declares own member: " + s.kindString + " " + s.nameString + " (" + s.id + ")")
            val traverser = new ExtractAll(Some(tree.symbol))
            traverser.traverse(rhs)
            
          case Template(parents, self, body) =>
            val parentTypeSymbols: Set[global.Symbol] = parents.map(parent => parent.tpe.typeSymbol).toSet
            println
            println(tree.tpe.typeSymbol.keyString + " " + tree.tpe.typeSymbol.nameString + " (" + tree.tpe.typeSymbol.id + ") ")
            if (defParent.isDefined) println("is owned by " + defParent.get.id)
            parentTypeSymbols.foreach(s => println("extends: " + s.keyString + " " + s.nameString + " (" + s.id + ") "))
            //tree.tpe.declarations.foreach(s => println("declares own member: " + s.kindString + " " + s.nameString + " (" + s.id + ")")) // TODO: need to deduplicate these
            val traverser = new ExtractAll(Some(tree.tpe.typeSymbol))
            body foreach { tree =>
              traverser.traverse(tree)
            }
            //traverseTrees(body)
          case tree =>
            println(" =========> general traverse call ")
            super.traverse(tree)
        }
      }
    }
    
    def newTraverse() = {
      val traverser = new ExtractAll(None)
      traverser.traverse(unit.body)
    }
    
    newTraverse()
  }
}
