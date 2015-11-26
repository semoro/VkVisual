import java.awt.event.{MouseWheelEvent, MouseWheelListener}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.net.URL
import java.util.function.Consumer
import javax.swing._

import org.graphstream.graph.Node
import org.graphstream.graph.implementations.{AbstractNode, MultiGraph}
import org.graphstream.ui.view.{Viewer, ViewerListener}

/**
  * XCodersTeam 2015 VkVisual
  * Created by semoro on 26.11.15.
  */


object ConstraintsHelper {
  def gridXY(x: Int, y: Int, gridBagConstraints: GridBagConstraints = new GridBagConstraints()): GridBagConstraints = {
    gridBagConstraints.gridx = x
    gridBagConstraints.gridy = y
    gridBagConstraints
  }

  def anchor(a: Int, gridBagConstraints: GridBagConstraints = new GridBagConstraints()): GridBagConstraints = {
    gridBagConstraints.anchor = a
    gridBagConstraints
  }
}

class Frame extends JFrame {

  private val loadingIcon = new ImageIcon(this.getClass.getResource("loading_spinner.gif"))

  val graph = new MultiGraph("VKVisual")
  val viewer = new Viewer(graph, Viewer.ThreadingModel.GRAPH_IN_GUI_THREAD)

  val viewerPipe = viewer.newViewerPipe()
  val listener = new ClickListener(graph)

  viewerPipe.addViewerListener(listener)
  viewerPipe.addSink(graph)


  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  graph.addAttribute("ui.stylesheet",
    "node .Male{ fill-color: blue; }\n" +
      "node .Female{fill-color:pink;}\n" +
      "node .Trap{fill-color:orange;}\n" +
      "node:clicked {fill-color:red;}\n" +
      "node .showInfo{fill-color:red;}\n")
  graph.addAttribute("ui.quality")
  graph.addAttribute("ui.antialias")

  var view = viewer.addDefaultView(false)

  setLayout(new BorderLayout)
  add(view, BorderLayout.CENTER)
  setSize(800, 600)

  val panel = new JPanel()
  add(panel, BorderLayout.WEST)

  panel.setLayout(new GridBagLayout)
  val name = new JLabel()
  panel.add(name, ConstraintsHelper.gridXY(0, 0))

  val photoIcon = new JLabel()
  panel.add(photoIcon, ConstraintsHelper.gridXY(0, 1))


  def setInfo(user: VKUser): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        name.setText(user.name)
        photoIcon.setIcon(loadingIcon)
      }
    })
    val i = new ImageIcon(new URL(user.photos(1)))
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = photoIcon.setIcon(i)
    })
  }
}


object Main {
  val frame = new Frame

  def main(args: Array[String]): Unit = {
    frame.setVisible(true)
    val viewer = frame.viewer
    val graph = frame.graph
    viewer.enableAutoLayout()
    viewer.getDefaultView.addMouseWheelListener(new MouseWheelListener {
      var rotation = 0.0

      override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
        rotation += e.getWheelRotation
        if (rotation <= 0)
          rotation = 1
        viewer.getDefaultView.getCamera.setViewPercent(rotation / 32)
      }
    })
    val me = VKApi.getUsers(Array("semoro")).head
    me.addNode(graph)
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        me.loadFriends()
        me.addNodes(graph)
      }
    })

    while (frame.listener.loop) {
      frame.viewerPipe.blockingPump()
    }

  }
}


class ClickListener(graph: MultiGraph) extends ViewerListener {
  var loop = true

  override def buttonReleased(id: String): Unit = {

  }

  override def buttonPushed(id: String): Unit = {
    val node: AbstractNode = graph.getNode(id)
    val vkUser: VKUser = node.getAttribute("user")
    graph.forEach(new Consumer[Node] {
      override def accept(t: Node): Unit = {
        val node: AbstractNode = t.asInstanceOf[AbstractNode]
        node.setAttribute("ui.class", node.getAttribute("user").asInstanceOf[VKUser].sex.toString)
      }
    })
    node.addAttribute("ui.class", "showInfo, " + node.getAttribute("user").asInstanceOf[VKUser].sex.toString)

    if (vkUser == null)
      return;
    if (node.hasAttribute("lastClick") && System.currentTimeMillis() - node.getAttribute("lastClick").asInstanceOf[Long] < 250) {
      vkUser.loadFriends()
      SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = vkUser.addNodes(graph)
      })
    } else {
      node.setAttribute("lastClick", Long.box(System.currentTimeMillis()))
      Main.frame.setInfo(vkUser)
    }
  }

  override def viewClosed(viewName: String): Unit = {
    loop = false
  }
}
