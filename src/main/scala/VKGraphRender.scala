import java.awt.event.{MouseWheelEvent, MouseEvent, MouseAdapter}
import java.awt._
import javax.swing.event.{PopupMenuEvent, PopupMenuListener}
import javax.swing.{JMenuItem, Action, JPopupMenu, JPanel}

import org.joml.Vector2f

/**
  * XCodersTeam 2015 VkVisual
  * Created by semoro on 28.12.15.
  */
class VKGraphRender(protected val uGraph: UGraph[VKUser]) extends JPanel {

  class MouseClickHandler extends MouseAdapter{
    override def mouseClicked(e: MouseEvent): Unit = {
      val pos = new Vector2f(((e.getX - px) / scl).toFloat, ((e.getY - py) / scl).toFloat)
      val node: Node[VKUser] = uGraph.nodes.find(node => node.pos.distance(pos) * scl < 10).orNull
      focusedNode = node
      if(focusedNode!=null && e.getButton == MouseEvent.BUTTON3)
      menu.show(VKGraphRender.this,e.getX,e.getY)
      if(focusedNode!=null && e.getButton == MouseEvent.BUTTON1)
        Main.frame.setInfo(focusedNode)
    }
    var d = 0.0
    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      d += e.getWheelRotation/8.0
      scl = Math.exp(d)
    }

    var drag = false
    var mx = 0
    var my = 0
    override def mouseDragged(e: MouseEvent): Unit = {
        px+=(e.getX-mx)
        py+=(e.getY-my)
        mx = e.getX
        my = e.getY

    }


    override def mousePressed(e: MouseEvent): Unit = {
      mx = e.getX
      my = e.getY
      drag=true
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      drag = false
    }
  }


  val menu = new JPopupMenu()

  PanelMenu.getMenu(this).foreach(menuItem => menu.add(menuItem))

  val mouseClickHandler = new MouseClickHandler
  this.addMouseListener(mouseClickHandler)
  this.addMouseWheelListener(mouseClickHandler)
  this.addMouseMotionListener(mouseClickHandler)

  var focusedNode:Node[VKUser] = null

  var px = 400
  var py = 200
  var scl = 1.0

  override def paint(g: Graphics): Unit = {

    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g2d.setRenderingHint(
      RenderingHints.KEY_FRACTIONALMETRICS,
      RenderingHints.VALUE_FRACTIONALMETRICS_ON)

    g.setColor(Color.WHITE)
    g.clearRect(0, 0, getWidth, getHeight)
    g.translate(px, py)
    val defaultStroke = new BasicStroke(1)
    val wStroke = new BasicStroke(3)
    uGraph.links foreach (link => {
      if (link.toNode(focusedNode)) {
        g2d.setStroke(wStroke)
        g.setColor(Color.GREEN)
      } else
        g.setColor(Color.DARK_GRAY)
      g.drawLine((link.a.pos.x * scl).toInt, (link.a.pos.y * scl).toInt, (link.b.pos.x * scl).toInt, (link.b.pos.y * scl).toInt)
      g2d.setStroke(defaultStroke)
    })

    uGraph.links filter (link => link.highlight) foreach (link => {
      g.setColor(Color.RED)
      g.drawLine((link.a.pos.x * scl).toInt, (link.a.pos.y * scl).toInt, (link.b.pos.x * scl).toInt, (link.b.pos.y * scl).toInt)
    })

    val searchName = Main.frame.search.getText.toLowerCase
    uGraph.nodes foreach (node => {
      g.setColor(Sex.getColor(node.data.sex))
      val size = if (searchName.length > 0 && node.data.name.toLowerCase.contains(searchName)) 2 else 1
      g.fillOval((node.pos.x * scl).toInt - 5 * size, (node.pos.y * scl).toInt - 5 * size, 10 * size, 10 * size)
      if(focusedNode == node) {
        g2d.setStroke(wStroke)
        g.setColor(Color.GREEN)
        g.drawOval((node.pos.x * scl).toInt - 5 * size, (node.pos.y * scl).toInt - 5 * size, 10 * size, 10 * size)
        g2d.setStroke(defaultStroke)
      }
    })

  }

}


