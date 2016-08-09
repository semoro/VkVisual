
import org.joml.Vector2f

import scala.collection.mutable


class UGraph[T] {
  val nodes: mutable.MutableList[Node[T]] = mutable.MutableList()
  val links: mutable.MutableList[Link[T]] = mutable.MutableList()

  def addEdge(a: T, b: T): Unit = {
    val k = new Link(nodes.find(node => node.data == a).get, nodes.find(node => node.data == b).get)

    if (!links.exists(link => link.equals(k)))
      links += k
  }

  def clearHighlightLinks(): Unit = {
    links foreach (link => link.highlight = false)
  }

  def findLow(a: Node[T], b: Node[T], quota: Int): List[Link[T]] = {
    if (quota == 0)
      links.filter(link => link.toNode(a) && link.toNode(b)).toList
    else
      links.filter(link => link.toNode(a)).filterNot(link => link.toNode(b))
        .map(link => if (link.a == a) link.b else link.a)
        .map(node => node)
        .map(node => findLow(node, b, quota - 1))
        .fold(List[Link[T]]())((b, a) => a ::: b)
  }

  def addVertex(a: T): Boolean = {
    nodes += new Node(a)
    true
  }

  def calculatePhysics(): Unit = {
    val tmp = new Vector2f
    nodes foreach (node => {
      nodes filterNot (a => a == node) foreach (s => {
        val dist = node.pos.distance(s.pos)
        if (dist < 50) {
          tmp.set(node.pos).sub(s.pos).normalize().mul(100 / dist)
          node.force.add(tmp)
          node.count += 1
        }
      })
    })


    links foreach (link => {
      val vec = tmp.set(link.a.pos).sub(link.b.pos)
      val d = -vec.length()
      link.a.force.add(vec.normalize().mul(d))
      link.a.count += 1
      link.b.force.add(vec.negate())
      link.b.count += 1
    })

    nodes foreach (node => {
      if (node.count > 0)
        node.pos.add(node.force.mul(0.5f / node.count))
      node.force.zero()
      node.count = 0
    })
  }
}

class PhysicsThread(graph: UGraph[AnyRef],callback: Runnable) extends Runnable{
  val thread = new Thread(this)
  var delta:Long = 0
  override def run(): Unit = {
    while(true) {
      val start = System.currentTimeMillis()
      graph.calculatePhysics()
      delta = System.currentTimeMillis() - start
      callback.run()
      if (delta < 50)
        Thread.sleep(50 - delta)
    }
  }
  def start(): Unit = {
    thread.start()
  }
}

class Node[T](_data: T) {
  var data: T = _data
  private val d = Math.random()
  var pos = new Vector2f(Math.sin(d * 2 * Math.PI).toFloat * 50, Math.cos(d * 2 * Math.PI).toFloat * 50)
  var force = new Vector2f()
  var count = 0
  var frozen = false
}

class Link[T](_a: Node[T], _b: Node[T]) {
  if (_a == null || _b == null)
    throw new RuntimeException
  val linkLength = 50

  var highlight = false
  var a = _a
  var b = _b

  def toNode(node: Node[T]): Boolean = a == node || b == node

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: Link[T] =>
        other.a == a && other.b == b || other.a == b && other.b == a
      case _ =>
        false
    }
  }
}

/**
  * XCodersTeam 2015 VkVisual
  * Created by semoro on 24.12.15.
  */
object UGraphHelper {
  def distance(a: Node[Any], b: Node[Any]): Double = a.pos.distance(b.pos)

  implicit class PowerInt(val i: Double) extends AnyVal {
    def **(exp: Double): Double = Math.pow(i, exp)

  }

}

