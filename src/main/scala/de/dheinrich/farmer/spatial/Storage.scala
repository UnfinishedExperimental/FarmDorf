package de.dheinrich.farmer.spatial

import scala.collection.immutable.TreeMap
import de.dheinrich.farmer.db.Village

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
  def getRange(range: (Int, Int)): Traversable[Village]
  def collectNodes(nodes: Traversable[Node]): Traversable[Village]
}

class TreeStorage(map: TreeMap[Int, Village]) extends Storage {
  def getRange(range: (Int, Int)) = {
    map.range(range._1, range._2 + 1).values
  }

  def collectNodes(nodes: Traversable[Node]): Traversable[Village] = {
    val buffer = IndexedSeq.newBuilder[Village]
    buffer.sizeHint(10000)

    for (n <- nodes) {
      buffer ++= getRange(n.range)
    }

    buffer.result
  }
}

class ArrayStorage(data: Array[Village]) extends Storage {
  def getRange(range: (Int, Int)) = {
    var iter = range._1

    val buffer = IndexedSeq.newBuilder[Village]
    buffer.sizeHint(10000)

    while (iter <= range._2) {
      iter = iter + 1
      val v = data(iter)
      if (v != null)
        buffer += v
    }

    buffer.result
  }

  def collectNodes(nodes: Traversable[Node]): Traversable[Village] = {
    val buffer = IndexedSeq.newBuilder[Village]
    buffer.sizeHint(10000)

    for (n <- nodes) {
      val (start, end) = n.range
      var iter = start
      while (iter <= end) {
        iter = iter + 1
        val v = data(iter)
        if (v != null)
          buffer += v
      }
    }

    buffer.result
  }
}