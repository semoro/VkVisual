import java.awt.event._
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.net.URL
import java.util.function.Consumer
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

import org.graphstream.graph.Node
import org.graphstream.graph.implementations.{AbstractNode, MultiGraph}
import org.graphstream.ui.layout.springbox.implementations.SpringBox
import org.graphstream.ui.view.{Viewer, ViewerListener}

class LoadingProgressBar extends JProgressBar {

  var str = ""
  var p: Int = 0
  var max = 0
  var linkType: LinkTypes.Value = null
  setStringPainted(true)

  def setState(linkType: LinkTypes.Value, max: Int): Unit = {
    this.max = max
    str = linkType.toString + " " + p + " из " + max
    this.linkType = linkType
    setString(str)
    p = 0
    setValue(p)
    setMaximum(max)
  }

  def updateProgress(): Unit = {
    p += 1
    str = linkType.toString + " " + p + " из " + max
    setString(str)
    setValue(p)
  }
}

class Frame extends JFrame {

  val graph = new MultiGraph("VKVisual")
  val viewer = new Viewer(graph, Viewer.ThreadingModel.GRAPH_IN_ANOTHER_THREAD)
  val viewerPipe = viewer.newViewerPipe()
  val listener = new ClickListener(graph)
  val graphLayout = new SpringBox()

  viewerPipe.addViewerListener(listener)
  viewerPipe.addSink(graph)


  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  graph.addAttribute("ui.stylesheet",
    "node .Male{ fill-color: blue; }\n" +
      "node .Female{fill-color:pink;}\n" +
      "node .Trap{fill-color:orange;}\n" +
      "node:clicked {fill-color:red;}\n" +
      "node .showInfo{fill-color:red;}\n" +
      "node .searchMatch{fill-color:green;}")
  //graph.addAttribute("ui.quality")
  graph.addAttribute("ui.antialias")
  val progressbar = new LoadingProgressBar()
  val panel = new JPanel()
  viewer.enableAutoLayout(graphLayout)

  setLayout(new BorderLayout)
  add(view, BorderLayout.CENTER)
  setSize(800, 600)
  val search = new JTextField()

  add(progressbar, BorderLayout.NORTH)
  val gbc = ConstraintsHelper.gridXY(0, 0)
  add(panel, BorderLayout.WEST)


  panel.setLayout(new GridBagLayout)
  val name = new JLabel()
  search.getDocument.addDocumentListener(new DocumentListener {
    override def insertUpdate(e: DocumentEvent): Unit = update()

    override def changedUpdate(e: DocumentEvent): Unit = update()

    override def removeUpdate(e: DocumentEvent): Unit = update()

    def update(): Unit = {
      println("Update " + search.getText)
      graph.getEachNode.forEach(new Consumer[Node] {
        override def accept(t: Node): Unit = {
          if (t.getAttribute("user").asInstanceOf[VKUser].name.contains(search.getText)) {
            t.addAttribute("ui.class", "searchMatch")
            println(t.getAttribute("user").asInstanceOf[VKUser].name)
          } else
            t.removeAttribute("ui.class")
        }
      })
    }
  })
  val photoIcon = new JLabel()
  gbc.fill = GridBagConstraints.HORIZONTAL
  panel.add(search, gbc)
  private val loadingIcon = new ImageIcon(this.getClass.getResource("loading_spinner.gif"))
  panel.add(name, ConstraintsHelper.gridXY(0, 1))
  var view = viewer.addDefaultView(false)
  panel.add(photoIcon, ConstraintsHelper.gridXY(0, 2))

  addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      super.keyPressed(e)
      if (e.getKeyCode == KeyEvent.VK_SPACE)
        view.getCamera.resetView()
    }
  })

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

class Worker(vKUser: VKUser) extends Runnable {
  override def run(): Unit = {
    vKUser.addDirectEdges
    vKUser.addIndirectEdges
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
      new Thread(new Worker(vkUser)).start()
    } else {
      node.setAttribute("lastClick", Long.box(System.currentTimeMillis()))
      Main.frame.setInfo(vkUser)
    }
  }

  override def viewClosed(viewName: String): Unit = {
    loop = false
  }
}

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


object LinkTypes extends Enumeration {
  val Direct = Value("Загрузка прямых связей")
  val Indirect = Value("Загрузка побочных связей")
}


object Main {
  val frame = new Frame
  val graph: MultiGraph = frame.graph
  def main(args: Array[String]): Unit = {
    System.setProperty("gs.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
    frame.setVisible(true)
    val viewer = frame.viewer
    graph.setStrict(false)
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

    val me = VKApi.getUsers(Array("16865527")).head
    me.addNode()
    frame.graphLayout.freezeNode(me.node.getId, true)
    me.loadFriends()
    new Worker(me).run()


    while (frame.listener.loop) {
      frame.viewerPipe.blockingPump()
    }

  }
}
