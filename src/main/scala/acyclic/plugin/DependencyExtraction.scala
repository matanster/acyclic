//acyclic
package acyclic.plugin
import acyclic.file
import scala.tools.nsc.Global
object DependencyExtraction{
  
  def apply(global: Global)(unit: global.CompilationUnit): Seq[(global.Symbol, global.Tree)] = {
    import global._

    @deprecated
    class CollectTypeTraverser[T](pf: PartialFunction[Type, T]) extends TypeTraverser {
      var collected: List[T] = Nil
      def traverse(tpe: Type): Unit = {
        if (pf.isDefinedAt(tpe))
          collected = pf(tpe) :: collected
        mapOver(tpe)
      }
    }

    class ExtractDependenciesTraverser extends Traverser {
      protected val depBuf = collection.mutable.ArrayBuffer.empty[(Symbol, Tree)]
      protected def addDependency(sym: Symbol, tree: Tree): Unit = depBuf += ((sym, tree))
      def dependencies: collection.immutable.Set[(Symbol, Tree)] = {
        // convert to immutable set and remove NoSymbol if we have one
        depBuf.toSet
      }
    }

    // this only accurately recovers the usage/inclusion hierarchy for naive AST - it does not really grasp
    // the correct hierarchies, only spits out `println`s per `traverse`s apparent depth-first traversal.
    // it was very helpful for getting going with rather benign test AST.
    @deprecated
    class ExtractDependenciesByMemberRefTraverser extends ExtractDependenciesTraverser {
      override def traverse(tree: Tree): Unit = {
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
                addDependency(lookupImported(name.toTermName), tree)
                addDependency(lookupImported(name.toTypeName), tree)
                //println(tree); println(lookupImported(name) + "(" + lookupImported(name).id + ")")
            }
          case select: Select =>
            addDependency(select.symbol, tree)
            ////println(select.tpe + " contains symbols: ")
            select.symbol.kindString match {
              case "constructor" => // ignore
              case "method" =>      println("uses: " + select.symbol + "(" + select.symbol.id + ")" +  " of " + select.symbol.owner + " owned by " + select.symbol.owner.owner)
              case _ =>             println("uses: " + select.symbol + " of type " + select.symbol.tpe.typeSymbol)
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
            addDependency(ident.symbol, tree)
            //println(ident.symbol)
          case typeTree: TypeTree  => // are we missing something by not handling this?
          
          case Template(parents, self, body) =>
            val parentTypeSymbols: Set[global.Symbol] = parents.map(parent => parent.tpe.typeSymbol).toSet
            println
            println(tree.tpe.typeSymbol.keyString + " " + tree.tpe.typeSymbol.nameString + " (" + tree.tpe.typeSymbol.id + ") ")
            println("is owned by " + tree.tpe.typeSymbol.owner + " owned by " + tree.tpe.typeSymbol.owner.owner)
            parentTypeSymbols.foreach(s => println("extends: " + s.keyString + " " + s.nameString + " (" + s.id + ") "))
            tree.tpe.declarations.foreach(s => println("declares own member: " + s.kindString + " " + s.nameString + "(" + s.id + ")")) // TODO: need to deduplicate these
            traverseTrees(body)
          case tree => super.traverse(tree)
        }
      }
    }

    def byMembers(): collection.immutable.Set[(Symbol, Tree)] = {
      val traverser = new ExtractDependenciesByMemberRefTraverser
      traverser.traverse(unit.body)
      traverser.dependencies
    }

    @deprecated
    class ExtractDependenciesByInheritanceTraverser extends ExtractDependenciesTraverser {
      override def traverse(tree: Tree): Unit = tree match {
        case Template(parents, self, body) =>
          // we are using typeSymbol and not typeSymbolDirect because we want
          // type aliases to be expanded
          val parentTypeSymbols: Set[global.Symbol] = parents.map(parent => parent.tpe.typeSymbol).toSet
          parentTypeSymbols.foreach(addDependency(_, tree))
          //println(tree.tpe.typeSymbol.keyString + " " + tree.tpe.typeSymbol.nameString + " (" + tree.tpe.typeSymbol.id + ") " + "extends:")
          //parentTypeSymbols.foreach(s => println(s.keyString + " " + s.nameString + " (" + s.id + ") "))
          //println
          traverseTrees(body)
        case tree => super.traverse(tree)
      }
    }

    @deprecated
    def byInheritence(): collection.immutable.Set[(Symbol, Tree)] = {
      val traverser = new ExtractDependenciesByInheritanceTraverser
      traverser.traverse(unit.body)
      traverser.dependencies
    }
    
    class ExtractAll(defParent: Option[global.Symbol]) extends ExtractDependenciesTraverser {
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
                addDependency(lookupImported(name.toTermName), tree)
                addDependency(lookupImported(name.toTypeName), tree)
                //println(tree); println(lookupImported(name) + "(" + lookupImported(name).id + ")")
            }
          case select: Select =>
            addDependency(select.symbol, tree)
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
            addDependency(ident.symbol, tree)
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
    
    def newTraverse(): collection.immutable.Set[(Symbol, Tree)] = {
      val traverser = new ExtractAll(None)
      traverser.traverse(unit.body)
      traverser.dependencies
    }
    
    newTraverse()
    //val traverseByMembers = byMembers()
    //val traverseByInheritence = byInheritence() 
    //(traverseByMembers | traverseByInheritence).toSeq // get object's dependencies arising from both what's inside 
                                                      // it and what's inherited by it (through `extend`).
    byInheritence().toSeq
  }
}
