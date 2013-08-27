package de.dheinrich.farmer.spatial

import de.dheinrich.farmer.db.Village

object QuadTree {
  val B = Array(0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF, 0x0000FFFF);
  val S = Array(1, 2, 4, 8);
  val Depth = 10

  require(Depth < 16)

  private val max = 1 << Depth
  def interleave(a: Int, b: Int): Int = {
    require(a < max)
    require(b < max)

    space(a) | (space(b) << 1)
  }

  def getCoord(id: Int, level: Int): (Int, Int) = {
    val localId = id >> ((Depth - level - 1) * 2)

    val x = unspace(localId)
    val y = unspace(localId >> 1)

    (x, y)
  }

  def space(a: Int): Int = {
    require(a < (1 << 16))

    var x = (a | (a << S(3))) & B(3)
    x = (x | (x << S(2))) & B(2)
    x = (x | (x << S(1))) & B(1)
    (x | (x << S(0))) & B(0)
  }

  def unspace(a: Int) = {
    var x = a & B(0)
    x = ((x >> S(0)) | x) & B(1)
    x = ((x >> S(1)) | x) & B(2)
    x = ((x >> S(2)) | x) & B(3)
    ((x >> S(3)) | x) & B(4)
  }
}

class QuadTree[A <: Storage](vills: Traversable[Village])(implicit builder: Builder[A]) {
  import QuadTree._

  private val vault: Storage = {
    val elements = vills.map(v => (getIndex(v), v))
    builder.buildFrom(elements)
  }

  private def getIndex(v: Village): Int = interleave(v.x, v.y)

  private def log2(a: Int) = (Integer.SIZE - 1) - Integer.numberOfLeadingZeros(a)

  def radiusSearch(x: Int, y: Int, r: Float): Traversable[Village] = {
    radiusSearch(Seq((x, y)), r)
  }

  def radiusSearch(points: Traversable[(Int, Int)], r: Float): Traversable[Village] = {

    val nodes = for ((x, y) <- points) yield {

      val area = Circle(x, y, r)

      val max = ((1 << Depth) / (2 * r)).ceil.toInt
      val depth = ((log2(max) min Depth) - 2) max -1 //-1 because our root is -1 and not 0

      val startNode = if (depth > 0)
        Node(Node.from(x, y, Depth - 1).id, depth)
      else
        Node.root

      search(startNode, area)
    }

    val t = nodes.flatten map { n => n.id -> n }
    val map = t.toMap

    vault.collectNodes(map.values)
  }

  private def search(node: Node, circle: Circle): Traversable[Node] = {
    if (circle.contains(node))
      node :: Nil
    else if (circle.overlap(node)) {
      node.children match {
        case Some(childs) => childs flatMap (n => search(n, circle))
        case None => node :: Nil
      }
    } else
      Nil
  }
}

object Node {
  import QuadTree._
  def from(x: Int, y: Int, level: Int) = {
    Node(interleave(x, y) << ((Depth - level - 1) * 2), level)
  }
  val root = Node(0, -1)
}

sealed abstract class Direction(val code: Int) {
  def |(o: Direction) = new Direction(code | o.code) {}
}

case object East extends Direction(1)
case object North extends Direction(2)
case object West extends Direction(QuadTree.space((1 << QuadTree.Depth) - 1))
case object South extends Direction(West.code << 1)

case class Node(id: Int, level: Int) {
  import QuadTree._

  require(level < Depth)

  def parent() = {
    if (level > 0)
      Node(id, level - 1)
    else
      Node.root
  }

  def children() = {
    val full = 0xFFFFFFFF
    if (level < Depth - 1) {
      val nl = level + 1
      val shift = 2 * (Depth - nl - 1)
      Some(
        for (i <- 0 to 3) yield {
          val mask = ((id >> shift) ^ i) & 3
          val nid = id ^ (mask << shift)
          Node(nid, nl)
        })
    } else
      None
  }

  def getCoord(): (Int, Int) = QuadTree.getCoord(id, level)

  def range() = {
    val s = 2 * (Depth - level - 1)
    val mask = (1 << s) - 1
    val negMask = ((1 << Depth * 2) - 1) ^ mask

    val start = id & negMask
    val end = id | mask

    (start, end)
  }

  def move(d: Direction): Node = {
    val tx = West.code
    val ty = South.code

    val moveCode = d.code << (2 * (Depth - level - 1))

    val x = ((id | ty) + (moveCode & tx)) & tx
    val y = ((id | tx) + (moveCode & ty)) & ty

    Node(x | y, level)
  }
}

case class Circle(x: Int, y: Int, r: Float) {
  import QuadTree._

  def contains(n: Node): Boolean = {
    val (nx, ny) = n.getCoord
    val s = Depth - n.level - 2

    var mx: Float = 0
    var my: Float = 0
    var h: Float = 0

    if (s < 0) {
      mx = nx + 0.5f
      my = ny + 0.5f
      h = 0.5f
    } else {
      mx = (2 * nx + 1) << s
      my = (2 * ny + 1) << s
      h = 1 << s
    }

    mx = mx - x
    my = my - y

    r * r >= mx * mx + my * my + 2 * h * h
  }

  def overlap(n: Node): Boolean =
    {
      val (nx, ny) = n.getCoord
      val s = Depth - n.level - 2

      var mx: Float = 0
      var my: Float = 0
      var h: Float = 0

      if (s < 0) {
        mx = nx + 0.5f
        my = ny + 0.5f
        h = 0.5f
      } else {
        mx = (2 * nx + 1) << s
        my = (2 * ny + 1) << s
        h = 1 << s
      }

      val cdx = (x - mx).abs
      val cdy = (y - my).abs

      val w = h + r

      if (!(cdx <= w && cdy <= w))
        return false

      if (!(cdx > h && cdy > h))
        return true

      val hx = cdx - h
      val hy = cdy - h

      hx * hx + hy * hy <= r * r
    }
}

  /*
 * 1024
 * 512
 * 256
 * 128
 * 64
 * 32
 * 16
 * 8
 * 4
 * 2
 * 1
 */
