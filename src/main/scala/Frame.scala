import java.awt._
import java.awt.event._
import java.net.URL
import javax.swing._

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

class Frame extends JFrame {

  val graph = new UGraph[VKUser]

  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  val progressbar = new LoadingProgressBar()
  val panel = new JPanel()

  setLayout(new BorderLayout)
  setSize(800, 600)
  val search = new JTextField()
  add(progressbar, BorderLayout.NORTH)

  val gbc = ConstraintsHelper.gridXY(0, 0)
  add(panel, BorderLayout.WEST)
  val name = new JTextField()


  val render = new VKGraphRender(graph)
  add(render, BorderLayout.CENTER)

  val physicsThread = new PhysicsThread(graph.asInstanceOf[UGraph[AnyRef]], new Runnable {
    override def run(): Unit = SwingUtilities.invokeLater(new Runnable() {
      override def run(): Unit = render.repaint()
    })
  })


  panel.setLayout(new GridBagLayout)
  val photoIcon = new JLabel()
  name.setEditable(false)



  gbc.fill = GridBagConstraints.HORIZONTAL
  panel.add(search, gbc)
  private val loadingIcon = new ImageIcon(this.getClass.getResource("loading_spinner.gif"))
  panel.add(name, ConstraintsHelper.gridXY(0, 1))

  panel.add(photoIcon, ConstraintsHelper.gridXY(0, 2))
  var scalaI = new JTextArea()

  var focusedNode: Node[VKUser] = null

  def setInfo(node: Node[VKUser]): Unit = {
    val user = node.data
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

  photoIcon.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (focusedNode != null) {
        val url = "http://vk.com/id" + focusedNode.data.id
        Runtime.getRuntime.exec("xdg-open " + url)
      }
    }
  })


}

class LoadTask(vKUser: VKUser) extends Runnable {
  override def run(): Unit = {
    vKUser.addDirectEdges
    vKUser.addIndirectEdges
  }
}

class WorkerThread extends Runnable {
  val thread = new Thread(this)

  override def run(): Unit = {
    import WorkerThread.taskQueue
    while (true) {
      taskQueue.synchronized {
        if (taskQueue.nonEmpty)
          taskQueue.dequeue.run()
        else
          taskQueue.wait()
      }
    }
  }

  def start(): Unit = {
    thread.start()
  }
}

object PanelMenu {
  def getMenu(vKGraphRender: VKGraphRender): scala.List[JMenuItem] = {
    val loadAllFriends = new JMenuItem("Подгрузить всех друзей")
    loadAllFriends.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        if (vKGraphRender.focusedNode != null)
          WorkerThread.addTask(new LoadTask(vKGraphRender.focusedNode.data))
      }
    })
    scala.List(loadAllFriends)
  }
}

object WorkerThread {
  val taskQueue: mutable.Queue[Runnable] = mutable.Queue()

  def addTask(runnable: Runnable): Unit = {
    taskQueue.enqueue(runnable)
    taskQueue.synchronized {
      taskQueue.notify()
    }
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

  var workers: scala.List[WorkerThread] = null

  def main(args: Array[String]): Unit = {
    frame.setVisible(true)


    val me = VKApi.getUsers(Array("semoro")).head
    me.addNode()
    me.loadFriends()
    frame.physicsThread.start()
    workers = (for (a <- 0 to 3) yield new WorkerThread).toList
    workers foreach (worker => worker.start())
    WorkerThread.addTask(new LoadTask(me))
  }
}
