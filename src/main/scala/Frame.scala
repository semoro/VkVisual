import java.awt._
import java.awt.event.{KeyEvent, KeyListener, MouseAdapter, MouseEvent}
import java.io._
import java.net.URL
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

import org.joml.Vector2f

import scala.collection.mutable


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
    import Main.frame.focusedNode
    uGraph.links foreach (link => {
      g.setColor(if (focusedNode == link.a || focusedNode == link.b) Color.ORANGE else Color.DARK_GRAY)
      g.drawLine((link.a.pos.x * scl).toInt, (link.a.pos.y * scl).toInt, (link.b.pos.x * scl).toInt, (link.b.pos.y * scl).toInt)
    })

    uGraph.links filter (link => link.highlight) foreach (link => {
      g.setColor(Color.RED)
      g.drawLine((link.a.pos.x * scl).toInt, (link.a.pos.y * scl).toInt, (link.b.pos.x * scl).toInt, (link.b.pos.y * scl).toInt)
    })

    val searchName = Main.frame.search.getText.toLowerCase
    uGraph.nodes foreach (node => {
      g.setColor(Sex.getColor(node.data.asInstanceOf[VKUser].sex))
      val size = if (searchName.length > 0 && node.data.asInstanceOf[VKUser].name.toLowerCase.contains(searchName)) 2 else 1
      g.fillOval((node.pos.x * scl).toInt - 5 * size, (node.pos.y * scl).toInt - 5 * size, 10 * size, 10 * size)
    })
  }

}

class KeyListen extends KeyListener {

  val pressedKeys = mutable.ArrayBuffer[Int]()

  override def keyTyped(e: KeyEvent): Unit = None

  override def keyPressed(e: KeyEvent): Unit = {
    pressedKeys += e.getKeyCode
  }

  override def keyReleased(e: KeyEvent): Unit = {
    pressedKeys -= e.getKeyCode
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
  val name = new JTextField()


  panel.setLayout(new GridBagLayout)
  val photoIcon = new JLabel()
  name.setEditable(false)
  search.getDocument.addDocumentListener(new DocumentListener {
    override def insertUpdate(e: DocumentEvent): Unit = update()

    override def changedUpdate(e: DocumentEvent): Unit = update()

    override def removeUpdate(e: DocumentEvent): Unit = update()

    def update(): Unit = {
      println("Update " + search.getText)

    }
  })

  render.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val pos = new Vector2f(((e.getX - render.px) / render.scl).toFloat, ((e.getY - render.py) / render.scl).toFloat)
      val node: Node[VKUser] = graph.nodes.find(node => node.pos.distance(pos) * render.scl < 10).orNull
      if (node != null) {
        if (e.getClickCount == 1)
          setInfo(node)
        else if (e.getClickCount == 2)
          new Thread(new Worker(node.data.asInstanceOf[VKUser])).start
      } else {
        focusedNode = null
      }

    }
  })
  val keyListen = new KeyListen
  gbc.fill = GridBagConstraints.HORIZONTAL
  panel.add(search, gbc)
  private val loadingIcon = new ImageIcon(this.getClass.getResource("loading_spinner.gif"))
  panel.add(name, ConstraintsHelper.gridXY(0, 1))

  panel.add(photoIcon, ConstraintsHelper.gridXY(0, 2))
  var scalaI = new JTextArea()
  search.addKeyListener(keyListen)
  var focusedNode: Node[VKUser] = null

  def setInfo(node: Node[VKUser]): Unit = {
    val user = node.data.asInstanceOf[VKUser]
    focusedNode = node
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        name.setText(user.name + "(" + user.id + ")")
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
        if (keyListen.pressedKeys.contains(KeyEvent.VK_UP))
          render.py += 50
        if (keyListen.pressedKeys.contains(KeyEvent.VK_DOWN))
          render.py -= 50
        if (keyListen.pressedKeys.contains(KeyEvent.VK_LEFT))
          render.px += 50
        if (keyListen.pressedKeys.contains(KeyEvent.VK_RIGHT))
          render.px -= 50
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
    frame.setVisible(true)


    val me = VKApi.getUsers(Array("mave4ka")).head
    me.addNode()
    me.loadFriends()
    new Thread(new Worker(me)).start()

    val reader = new BufferedReader(new InputStreamReader(System.in))
    while (true) {
      val cmd = reader.readLine().split(" ")
      cmd.head match {
        case "wl" => {
          val idA = BigInt(cmd(1))
          val idB = BigInt(cmd(2))
          val quota = cmd(3).toInt
          graph.findLow(graph.nodes.find(node => node.data.asInstanceOf[VKUser].id == idA).get,
            graph.nodes.find(node => node.data.asInstanceOf[VKUser].id == idB).get, quota)
            .foreach(link => link.highlight = true)
        }
        case "clr" => graph.clearHighlightLinks()

      }
    }

  }
}
