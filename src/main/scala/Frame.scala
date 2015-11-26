import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.net.URL
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

import com.mxgraph.layout.mxOrganicLayout
import com.mxgraph.swing.mxGraphComponent
import org.jgrapht.ext.JGraphXAdapter
import org.jgrapht.graph.{DefaultEdge, ListenableUndirectedGraph}

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

  val graph = new ListenableUndirectedGraph[VKUser, DefaultEdge](classOf[DefaultEdge])

  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  val progressbar = new LoadingProgressBar()
  val panel = new JPanel()
  val jgxAdapter = new JGraphXAdapter[VKUser, DefaultEdge](graph)
  setLayout(new BorderLayout)
  add(new mxGraphComponent(jgxAdapter))
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
  panel.add(search, gbc)
  private val loadingIcon = new ImageIcon(this.getClass.getResource("loading_spinner.gif"))
  panel.add(name, ConstraintsHelper.gridXY(0, 1))
  // var view = viewer.addDefaultView(false)
  panel.add(photoIcon, ConstraintsHelper.gridXY(0, 2))


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
    Main.layout.execute(Main.jgxAdapter.getDefaultParent())
    Main.fitGraph()
    vKUser.addIndirectEdges
    Main.layout.execute(Main.jgxAdapter.getDefaultParent())
    Main.fitGraph()
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
  val graph: ListenableUndirectedGraph[VKUser, DefaultEdge] = frame.graph
  val jgxAdapter = frame.jgxAdapter
  val layout = new mxOrganicLayout(jgxAdapter)

  def fitGraph(): Unit = {
    val view = jgxAdapter.getView()
    val compLen = 800
    val viewLen = view.getGraphBounds().getWidth()
    view.setScale(compLen / viewLen * view.getScale())
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("gs.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
    frame.setVisible(true)



    val me = VKApi.getUsers(Array("16865527")).head
    me.addNode()
    me.loadFriends()
    new Worker(me).run()


  }
}
