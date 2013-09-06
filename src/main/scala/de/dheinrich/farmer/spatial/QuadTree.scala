package de.dheinrich.farmer.spatial

import de.dheinrich.farmer.db.Village
import scala.annotation.tailrec
import shapeless.headOption

object QuadTree {
  val B = Array(0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF, 0x0000FFFF);
  val S = Array(1, 2, 4, 8);
  val Depth = 10
  val Density = 0.2

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
      val startNode = smalestEnclosingNode(area)

      search(startNode, area)
    }

    val t = nodes.flatten map { n => n.id -> n }
    val map = t.toMap

    vault.collectNodes(map.values)
  }

  private def smalestEnclosingNode(c: Circle) = {

    val max = ((1 << Depth) / (2 * c.r)).ceil.toInt
    val depth = ((log2(max) min Depth) - 2) max -1 //-1 because our root is -1 and not 0

    if (depth > 0)
      Node(Node.from(c.x, c.y, Depth - 1).id, depth)
    else
      Node.root
  }

  def areaSearch(x: Int, y: Int, width: Int, height: Int) = {
    val nodes = search(Node.root, AABB(x, y, width, height))
    vault.collectNodes(nodes)
  }

  private def search(node: Node, area: SearchArea): Seq[Node] = {
    if (area.contains(node))
      node :: Nil
    else if (area.overlap(node)) {
      node.children match {
        case Some(childs) => childs flatMap (n => search(n, area))
        case None => node :: Nil
      }
    } else
      Nil
  }

  //    def nearest2(x: Int, y: Int) = {
  //      val first = vault.traverseNodes(Seq(Node.root)).headOption
  //  
  //      first map { v =>
  //        val firstDist = dist(x, y, v)
  //        nearerThen(x, y, firstDist, v)
  //      }
  //    }
  //  
  //    @tailrec
  //    private def nearerThen(x: Int, y: Int, r: Float, last: Village): Village = {
  //      val area = Circle(x, y, r)
  //  
  //      val startNode = smalestEnclosingNode(area)
  //      if (startNode.level == Depth - 1)
  //        return last
  //  
  //      val supNodes = search(startNode, area)
  //  
  //      val villages = vault.traverseNodes(Seq(Node.root)).view
  //      val next = villages.
  //        filter(_ != last).
  //        map(v => (v, dist(x, y, v))).
  //        filter(_._2 < r).headOption
  //  
  //      next match {
  //        case None => last
  //        case Some((v, ra)) => nearerThen(x, y, ra, last)
  //      }
  //    }

  def nearest(x: Int, y: Int) = {
    nearestReq(x, y, Node.leafAt(x, y))
  }

  private val allDir = Seq(East, East | North, North, West | North, West, West | South, South, East | South)
  private def nearestReq(x: Int, y: Int, start: Node): Village = {
    val nodes = allDir.map(start.move(_))
    val vills = vault.collectNodes(start +: nodes)
    if (vills.size >= 0)
      vills.toSeq.sortBy(distSqr(x, y, _)).head
    else
      nearestReq(x, y, start.parent)
  }

  private val VILL_COUNT_EXP = 30
  private val START_RADIUS = (VILL_COUNT_EXP / (Density * Math.PI)).toFloat

  def nearestStream(x: Int, y: Int): Stream[Village] = {

    def a(r: Float, lastR: Float, last: Seq[Village] = Seq.empty): Stream[Village] = {
      if (last.isEmpty) {
        val area = Ring(x, y, lastR, r)

        val node = smalestEnclosingNode(area.big)
        val nodes = search(node, area)

        val vill = vault.collectNodes(nodes).sortBy(v => distSqr(x, y, v))

        if (nodes.length == 1 && nodes(0) == Node.root)
          vill.toStream
        else
          a(r * 1.6f, r, vill)
      } else {
        Stream.cons(last.head, a(r, lastR, last.tail))
      }
    }

    a(START_RADIUS, 0)
  }

  def dist(x: Int, y: Int, v: Village) = {
    Math.sqrt(distSqr(x, y, v)).toFloat
  }

  def distSqr(x: Int, y: Int, v: Village) = {
    val w = x - v.x
    val h = y - v.y
    w * w + h * h
  }
}

object Node {
  import QuadTree._
  def from(x: Int, y: Int, level: Int) = {
    Node(interleave(x, y) << ((Depth - level - 1) * 2), level)
  }
  def leafAt(x: Int, y: Int) = from(x, y, Depth - 1)
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

    start to end
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

trait SearchArea {
  def contains(n: Node): Boolean
  def overlap(n: Node): Boolean
}

object AABB {
  def from(n: Node) = {
    val (nx, ny) = n.getCoord
    val s = QuadTree.Depth - n.level - 2

    if (s < 0) {
      AABB(nx, ny, 1, 1)
    } else {
      val mx = (2 * nx + 1) << s
      val my = (2 * ny + 1) << s
      val h = 1 << s
      AABB(mx - h, my - h, h * 2, h * 2)
    }
  }
}

case class AABB(x: Int, y: Int, width: Int, height: Int) extends SearchArea {
  def contains(n: Node): Boolean = contains(AABB.from(n))
  def overlap(n: Node): Boolean = overlap(AABB.from(n))

  def contains(o: AABB): Boolean = o.x >= x && o.y >= y && o.x + o.width <= x + width && o.y + o.height <= y + height
  def overlap(o: AABB): Boolean = {
    val inx = (o.x + o.width > x) && (o.x < x + width)
    val iny = (o.y + o.height > y) && (o.y < y + height)

    inx && iny
  }
}

case class Ring(x: Int, y: Int, min: Float, max: Float) extends SearchArea {
  val small = Circle(x, y, min)
  val big = Circle(x, y, max)

  def contains(n: Node) = big.contains(n) && !small.contains(n)
  def overlap(n: Node) = big.overlap(n) && !small.contains(n)
}

case class Circle(x: Int, y: Int, r: Float) extends SearchArea {
  import QuadTree._

  val r2 = r * r

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

    r2 >= mx * mx + my * my + 2 * h * h
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

      hx * hx + hy * hy <= r2
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
