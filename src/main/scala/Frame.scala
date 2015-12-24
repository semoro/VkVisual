import java.awt._
import java.awt.event.{KeyEvent, KeyListener}
import java.net.URL
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}


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

class RenderOO(uGraph: UGraph[VKUser]) extends JPanel {

  var px = 400
  var py = 200
  var scl = 4.0


  override def paint(g: Graphics): Unit = {
    g.setColor(Color.WHITE)
    g.clearRect(0, 0, getWidth, getHeight)
    g.translate(px, py)
    g.setColor(Color.BLACK)
    uGraph.links foreach (link => g.drawLine((link.a.pos.x * scl).toInt, (link.a.pos.y * scl).toInt, (link.b.pos.x * scl).toInt, (link.b.pos.y * scl).toInt))
    g.setColor(Color.RED)
    uGraph.nodes foreach (node => {
      g.fillRect((node.pos.x * scl).toInt - 5, (node.pos.y * scl).toInt - 5, 10, 10)
    })
  }

  def fit(): Unit = {

  }
}

class Frame extends JFrame {

  val graph = new UGraph[VKUser]

  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  val progressbar = new LoadingProgressBar()
  val panel = new JPanel()
  val render = new RenderOO(graph)
  setLayout(new BorderLayout)
  render.setSize(800, 600)
  add(render, BorderLayout.CENTER)
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
    }
  })
  val photoIcon = new JLabel()
  gbc.fill = GridBagConstraints.HORIZONTAL
  //panel.add(search, gbc)
  private val loadingIcon = new ImageIcon(this.getClass.getResource("loading_spinner.gif"))
  panel.add(name, ConstraintsHelper.gridXY(0, 1))
  // var view = viewer.addDefaultView(false)
  panel.add(photoIcon, ConstraintsHelper.gridXY(0, 2))

  this.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = None

    override def keyPressed(e: KeyEvent): Unit = None

    override def keyReleased(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_SPACE) {

        render.repaint()
        graph.nodes foreach (node => println(node.pos.x + "," + node.pos.y))
      }
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

  new Thread(new Runnable {
    override def run(): Unit = {
      while (true) {
        graph.calculatePhys()
        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = render.repaint()
        })

        Thread.sleep(50)
      }
    }
  }).start()
}

class Worker(vKUser: VKUser) extends Runnable {
  override def run(): Unit = {
    vKUser.addDirectEdges
    vKUser.addIndirectEdges
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
  val graph: UGraph[VKUser] = frame.graph


  def main(args: Array[String]): Unit = {
    System.setProperty("gs.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
    frame.setVisible(true)



    val me = VKApi.getUsers(Array("16865527")).head
    me.addNode()
    me.loadFriends()
    new Worker(me).run()


  }
}
