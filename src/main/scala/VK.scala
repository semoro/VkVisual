

import java.awt.{Color, Desktop}
import java.io.{File, FileWriter}
import javax.swing.JOptionPane

import com.github.scribejava.apis.VkontakteApi
import com.github.scribejava.core.builder.ServiceBuilder
import com.github.scribejava.core.model.{OAuthRequest, Verb, Verifier}
import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import org.json4s.FileInput
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

import scala.collection.mutable

class VKUser(val name: String, val id: BigInt, val sex: Sex.Value, val photos: List[String]) {
  var friends: List[VKUser] = List()
  var links: mutable.MutableList[VKUser] = mutable.MutableList()

  var node = false

  override def toString() = name + "(" + id + ") " + sex

  def addDirectEdges: Unit = {
    Main.frame.progressbar.setState(LinkTypes.Indirect, friends.count(friend => !friend.node))
    friends.filter(friend => !friend.node).filter(friend => !links.contains(friend)).foreach(friend => {
      friend.addNode()
      Main.graph.addEdge(this, friend)
      links += friend
      friend.links += this
      Main.frame.progressbar.updateProgress()
      None
    })
  }

  def addNode(): Unit = {
    node = Main.graph.addVertex(this)
  }

  def addIndirectEdges: Unit = {
    Main.frame.progressbar.setState(LinkTypes.Indirect, friends.count(f => true))
    friends.foreach(friend => {
      try {
        friend.loadFriends()
        friend.addAvailableEdges
        Main.frame.progressbar.updateProgress()
      } catch {
        case e: Exception => e.printStackTrace()
      }
      None
    })
  }

  def loadFriends(): Unit = {
    if (friends.isEmpty)
      friends :::= VKApi.getFriends(this)
  }

  def addAvailableEdges: Unit = {
    friends.filter(friend => friend.node).filter(friend => !links.contains(friend)).foreach(friend => {
      Main.graph.addEdge(this, friend)
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
    val result = getS(url)
    //println(result)
    println("Response")
    new FileWriter(cachedVersion).append(result).close()
    JsonMethods.parse(result)
  }

  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  def getS(url: String, requestMethod: String = "GET") = {
    import VKApi.service
    val request = new OAuthRequest(Verb.GET, url, service)
    service.signRequest(VKApi.accessToken, request)
    request.send().getBody
  }
}


object Sex extends Enumeration {
  val Trap, Female, Male = Value


  def getColor(v: Sex.Value): Color = {
    v match {
      case Sex.Male => new Color(0x01adef)
      case Sex.Female => new Color(0xe90090)
      case Sex.Trap => new Color(0x9ec52c)
    }
  }
}

object VKApi {


  val allUsers: mutable.MutableList[VKUser] = mutable.MutableList()
  val apivkcom = "https://api.vk.com/method"
  val API_VERSION = "5.40"
  val clientId = "5163283"
  val clientSecret = "D8Z3poNzug4AwS8Lqjqu"
  val service = new ServiceBuilder()
    .provider(classOf[VkontakteApi])
    .apiKey(clientId)
    .apiSecret(clientSecret)
    .scope("friends") // replace with desired scope
    .callback("http://oauth.vk.com/blank.html")
    .connectTimeout(1000)
    .readTimeout(1000)
    .build()
  val authorizationUrl = service.getAuthorizationUrl(null)

  // Obtain the Authorization URL
  System.out.println("Fetching the Authorization URL...")
  val code = JOptionPane.showInputDialog("And paste the authorization code here")
  System.out.println("Got the Authorization URL!")
  System.out.println("Now go and authorize ScribeJava here:")
  System.out.println(authorizationUrl)
  System.out.println("And paste the authorization code here")
  System.out.print(">>")
  val verifier = new Verifier(code)
  val accessToken = service.getAccessToken(null, verifier)

  // Trade the Request Token and Verfier for the Access Token
  System.out.println("Trading the Request Token for an Access Token...")

  def openWebpage(uri: Uri): Unit = {
    val desktop = if (Desktop.isDesktopSupported()) Desktop.getDesktop() else null
    if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
      desktop.browse(uri.toURI)

    }
    println(uri.toURI)
  }

  System.out.println("Got the Access Token!")
  System.out.println("(if your curious it looks like this: " + accessToken + " )")

  def getUsers(ids: Array[String]): List[VKUser] = {
    val v = REST.get(apivkcom + "users.get" ? ("version" -> API_VERSION) & ("user_ids" -> ids.mkString(",")) & ("name_case" -> "Nom") & ("fields" -> Array("sex", "photo_50", "photo_200").mkString(",")))
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