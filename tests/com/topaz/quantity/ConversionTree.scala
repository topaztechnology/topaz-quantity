package com.topaz.quantity

import com.topaz.TopazCodingError
import com.topaz.quantity.ConversionTree._

/**
 * The map is a
 *  T -> relationships[T]
 * The relationships describe if the links are that of a are parent or child.
 * The map will contain both directions, for example a single conversion will be:
 * Map(
 *      "a" -> DirectedLink("b", Child, 2),
 *      "b" -> DirectedLink("a", Parent, 1/2)
 *    )
 *
 * Use ConversionTreeBuilder to build an instance of this class
 */
class ConversionTree[T] private(map: Map[T, List[DirectedLink[T]]]) {
  private def roots = map.filter {
    case (k, connections) => connections.forall(_.direction == Child)
  }.keys
  require(map.isEmpty || roots.nonEmpty, "No root implies a cycle: " + map)
  verifyNoCycle()

  private def verifyNoCycle(): Unit = {
    // depth first search of the conversion tree
    roots.foreach {
      t =>
        var seen = List[T]()
        var children = childrenOf(t)
        while (children.nonEmpty) {
          children.foreach {
            child =>
              if (seen.contains(child.item))
                throw TopazCodingError("Already seen " + child.item)
              seen ::= child.item
          }
          children = children.flatMap(c => childrenOf(c.item))
        }
    }
  }

  def conversion(from: T, to: T): Option[BigDecimal] = {
    // mutually recursive tree walking that uses TailCalls to help show the scala compiler it is tail recursive
    import scala.util.control.TailCalls._
    type Forest = List[DirectedLink[T]]
    type Tree = DirectedLink[T]
    type Breadcrumbs = List[DirectedLink[T]]

    def treeRecurse(parent: T, tree: Tree, linkToHere: Breadcrumbs): TailRec[Breadcrumbs] = {
      if (tree.item == to) {
        done(tree :: linkToHere)
      } else {
        val forest = connectionsOf(tree.item, exclude = parent)
        tailcall(forestRecurse(tree.item, forest, tree :: linkToHere))
      }
    }

    def forestRecurse(parent: T, forest: Forest, linkToHere: Breadcrumbs): TailRec[Breadcrumbs] = {
      forest match {
        case Nil =>
          done(Nil)
        case tree :: restOfForest =>
          for {
            x <- treeRecurse(parent, tree, linkToHere)
            y <- forestRecurse(parent, restOfForest, linkToHere)}
          yield {
            x ::: y
          }
      }
    }
    val link = forestRecurse(from, map.getOrElse(from, Nil), Nil).result

    link match {
      case Nil => None
      case _ =>
        Some(link.map(_.conversion).product)
    }
  }

  private def childrenOf(t: T) = map.getOrElse(t, Nil).filter(_.direction == Child)
  private def connectionsOf(t: T, exclude: T) = map.getOrElse(t, Nil).filterNot(_.item == exclude)
}

object ConversionTree {
  sealed trait Relationship {
    def reverse: Relationship
  }

  case object Child extends Relationship {
    override def reverse: Relationship = Parent
  }

  case object Parent extends Relationship {
    override def reverse: Relationship = Child
  }

  case class DirectedLink[T](item: T, direction: Relationship, conversion: BigDecimal) {
    def reverse(newItem: T) = copy(item = newItem, direction = direction.reverse, conversion = 1 / conversion)
  }


  class ConversionTreeBuilder[T] {
    var map: Map[T, List[DirectedLink[T]]] = Map()

    private def append(from: T, link: DirectedLink[T]) = {
      map + map.get(from).map {
        entries => from -> (link :: entries)
      }.getOrElse(from -> List(link))
    }

    def add(from: T, to: T, conversion: BigDecimal) = {
      val link = DirectedLink(to, Child, conversion)
      map = append(from, link)
      map = append(link.item, link.reverse(from))
    }

    def build: ConversionTree[T] = {
      new ConversionTree(map)
    }
  }
}
