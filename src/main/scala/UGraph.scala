
import org.joml.Vector2f

class UGraph[T] {
  val nodes: Set[Node[T]] = Set()
  val links: Set[Link] = Set()

  def calculatePhys(): Unit = {
    links foreach (link => {

    })
  }
}

class Node[T](_data: T) {
  var data: T = _data
  var pos = new Vector2f()
  var force = new Vector2f()
  var count = 0
  var frozen = false
}

class Link(_a: Node, _b: Node) {
  val linkLength = 50

  var a = _a
  var b = _b


  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: Link =>
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
  def distance(a: Node, b: Node): Double = a.pos.distance(b.pos)

  implicit class PowerInt(val i: Double) extends AnyVal {
    def **(exp: Double): Double = Math.pow(i, exp)

  }

}