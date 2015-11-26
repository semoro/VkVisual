

import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import org.graphstream.graph.implementations.{MultiGraph, AbstractNode}
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

import scala.collection.mutable

/**
  * XCodersTeam 2015 VkVisual
  * Created by semoro on 26.11.15.
  */

object REST {
  def get(url: Uri): JValue = {
    println(url.toString)
    val result = scala.io.Source.fromURL(url.toString)("UTF-8").mkString
    println(result)
    JsonMethods.parse(result)
  }
}


object Sex extends Enumeration {
  val Trap, Female, Male = Value
}


class VKUser(_name: String, uid: BigInt, _sex: Sex.Value, _photos:List[String]) {
  override def toString() = name + "(" + uid + ") " + sex

  val name = _name
  var friends: List[VKUser] = List()
  var friendsDisplayed = false
  val id = uid
  val sex = _sex
  var node: AbstractNode = null
  val photos = _photos

  def loadFriends(): Unit = {
    if (!friendsDisplayed)
      friends :::= VKApi.getFriends(this)
  }


  def addNode(graph: MultiGraph): Unit = {
    node = graph.addNode(id.toString())
    node.setAttribute("user", this)
    node.setAttribute("ui.class", sex.toString)
  }

  def addNodes(graph: MultiGraph): Unit = {
    if (friendsDisplayed)
      return
    friendsDisplayed = true
    friends.foreach(user => {
      if (user.node == null) {
        user.addNode(graph)
      }
      graph.addEdge(this.id + "-" + user.id, this.id.toString(), user.id.toString())
      None
    })
  }

  VKApi.allUsers+=this
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