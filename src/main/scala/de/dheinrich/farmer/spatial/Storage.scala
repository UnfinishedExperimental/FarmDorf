package de.dheinrich.farmer.spatial

import scala.collection.immutable.TreeMap
import de.dheinrich.farmer.db.Village
import scala.annotation.tailrec
import scala.collection.LinearSeq

trait Builder[A <: Storage] {
  def buildFrom(vills: Traversable[(Int, Village)]): A
}

object Storage {
  implicit object TreeStorageBuilder extends Builder[TreeStorage] {
    def buildFrom(vills: Traversable[(Int, Village)]) = {
      val builder = TreeMap.newBuilder[Int, Village](Ordering.Int)
      builder.sizeHint(vills.size)

      vills foreach builder.+=

      new TreeStorage(builder.result)
    }
  }

  implicit object ArrayStorageBuilder extends Builder[ArrayStorage] {
    def buildFrom(vills: Traversable[(Int, Village)]) = {
      val data = new Array[Village](1 << (QuadTree.Depth * 2))

      for ((id, v) <- vills) {
        data(id) = v
      }

      new ArrayStorage(data)
    }
  }
}

trait Storage {
  def getRange(range: Range): Traversable[Village]
  def collectNodes(nodes: Traversable[Node]): Seq[Village]
  def traverseNodes(nodes: Traversable[Node]): Traversable[Village]
}

class TreeStorage(map: TreeMap[Int, Village]) extends Storage {
  def getRange(range: Range) = {
    map.range(range.start, range.last + 1).values
  }

  def collectNodes(nodes: Traversable[Node]): Seq[Village] = {
    val buffer = IndexedSeq.newBuilder[Village]
    buffer.sizeHint(2048)

    val ranges = merge(nodes.map(_.range).toList)
    for (n <- ranges) {
      buffer ++= getRange(n)
    }

    buffer.result
  }

  def traverseNodes(nodes: Traversable[Node]): Traversable[Village] = {
    nodes.view flatMap { n =>
      val r = n.range
      map.range(r.start, r.last + 1).valuesIterator
    }
  }

  @tailrec
  private def collapse(rs: List[Range], sep: List[Range] = Nil): List[Range] = rs match {
    case x :: y :: rest =>
      //    case x :: y :: rest =>
      if (y.start > x.last + 1) collapse(y :: rest, x :: sep)
      else collapse((x.start to (x.last max y.last)) :: rest, sep)
    case _ =>
      rs ::: sep
  }

  def merge(rs: List[Range]) = collapse(rs.sortBy(_.start))
}

class ArrayStorage(data: Array[Village]) extends Storage {
  def getRange(range: Range) = {
    val buffer = IndexedSeq.newBuilder[Village]
    buffer.sizeHint(2048)

    for (iter <- range) {
      val v = data(iter)
      if (v != null)
        buffer += v
    }

    buffer.result
  }

  def collectNodes(nodes: Traversable[Node]): Seq[Village] = {
    val buffer = IndexedSeq.newBuilder[Village]
    buffer.sizeHint(2048)

    for (n <- nodes) {
      val r = n.range
      var iter = r.start
      while (iter <= r.last) {
        iter = iter + 1
        val v = data(iter)
        if (v != null)
          buffer += v
      }
    }

    buffer.result
  }

  def traverseNodes(nodes: Traversable[Node]): Traversable[Village] = {
    (for (
      n <- nodes.view;
      i <- n.range
    ) yield {
      data(i)
    }) filter (_ != null)
  }
}