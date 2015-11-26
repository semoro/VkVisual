

import java.io.{File, FileWriter}

import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import org.graphstream.graph.implementations.AbstractNode
import org.json4s.FileInput
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

import scala.collection.mutable

class VKUser(val name: String, val id: BigInt, val sex: Sex.Value, val photos: List[String]) {
  var friends: List[VKUser] = List()
  var links: mutable.MutableList[VKUser] = mutable.MutableList()
  var node: AbstractNode = null

  override def toString() = name + "(" + id + ") " + sex

  def addDirectEdges: Unit = {
    Main.frame.progressbar.setState(LinkTypes.Indirect, friends.count(friend => friend.node == null))
    friends.filter(friend => friend.node == null).filter(friend => !links.contains(friend)).foreach(friend => {
      friend.addNode()
      Main.graph.addEdge(id + "-" + friend.id, node, friend.node)
      links += friend
      friend.links += this
      Main.frame.progressbar.updateProgress()
      None
    })
  }

  def addNode(): Unit = {
    node = Main.graph.addNode(id.toString())
    node.setAttribute("user", this)
    node.setAttribute("ui.class", sex.toString)
    node.setAttribute("layout.weight", "0")
  }

  def addIndirectEdges: Unit = {
    Main.frame.progressbar.setState(LinkTypes.Indirect, friends.count(f => true))
    friends.foreach(friend => {
      friend.loadFriends()
      friend.addAvailableEdges
      Main.frame.progressbar.updateProgress()
    })
  }

  def loadFriends(): Unit = {
    if (friends.isEmpty)
      friends :::= VKApi.getFriends(this)
  }

  def addAvailableEdges: Unit = {
    friends.filter(friend => friend.node != null).filter(friend => !links.contains(friend)).foreach(friend => {
      Main.graph.addEdge(id + "-" + friend.id, node, friend.node)
      links += friend
      friend.links += this
      None
    })
  }

  VKApi.allUsers += this
}


/**
  * XCodersTeam 2015 VkVisual
  * Created by semoro on 26.11.15.
  */

object REST {
  def get(url: Uri): JValue = {
    val str = url.toString
    println(str)
    val cachedVersion = new File("cache/" + str.substring(str.indexOf("//") + 2))
    if (cachedVersion.exists()) {
      println("Loaded from cache")
      return JsonMethods.parse(new FileInput(cachedVersion))
    }
    val result = scala.io.Source.fromURL(str).mkString
    //println(result)
    println("Response")
    new FileWriter(cachedVersion).append(result).close()
    JsonMethods.parse(result)
  }
}


object Sex extends Enumeration {
  val Trap, Female, Male = Value
}

object VKApi {

  val allUsers:mutable.MutableList[VKUser] = mutable.MutableList()

  val apivkcom = "https://api.vk.com/method"
  val API_VERSION = "5.40"

  def getUsers(ids: Array[String]): List[VKUser] = {
    val v = REST.get(apivkcom + "users.get" ? ("version" -> API_VERSION) & ("user_ids" -> ids.mkString(",")) & ("name_case" -> "Nom") & ("fields" -> Array("counters", "sex", "photo_50", "photo_200").mkString(",")))
    println(v \ "response")
    for (JObject(u) <- v \ "response";
                JField("first_name", JString(firstName)) <- u;
                JField("last_name", JString(lastName)) <- u;
                JField("uid", JInt(uid)) <- u;
                JField("sex", JInt(sexId)) <- u;
                JField("photo_50", JString(photo50)) <- u;
                JField("photo_200", JString(photo200)) <- u
    ) yield allUsers.find(user => user.id == uid).getOrElse(new VKUser(firstName + " " + lastName, uid, Sex.values.find(s => s.id == sexId).get, List(photo50,photo200)))
  }

  def getFriends(user: VKUser): List[VKUser] = {
    val v = REST.get(apivkcom + "friends.get" ? ("version" -> API_VERSION) & ("offset" -> 0) & ("count" -> 2000) & ("user_id" -> user.id) & ("name_case" -> "Nom") & ("fields" -> Array("sex", "photo_50", "photo_200").mkString(",")))
    for (JObject(u) <- v \ "response";
                JField("first_name", JString(firstName)) <- u;
                JField("last_name", JString(lastName)) <- u;
                JField("uid", JInt(uid)) <- u;
                JField("sex", JInt(sexId)) <- u;
                JField("photo_50", JString(photo50)) <- u;
                JField("photo_200", JString(photo200)) <- u
    ) yield allUsers.find(user => user.id == uid).getOrElse(new VKUser(firstName + " " + lastName, uid, Sex.values.find(s => s.id == sexId).get, List(photo50,photo200)))
  }
}