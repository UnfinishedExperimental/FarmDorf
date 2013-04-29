package de.dheinrich.farmer

import scala.Option.option2Iterable
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Buffer
import com.ning.http.client.Cookie
import com.ning.http.client.RequestBuilder
import com.ning.http.client.Response
import de.dheinrich.farmer.db.Village
import de.dheinrich.farmer.json.JsonMapper
import de.dheinrich.farmer.json.JsonMapSektor
import dispatch.Defaults.executor
import dispatch.Future
import dispatch.Future
import dispatch.Http
import dispatch.as
import dispatch.host
import dispatch.implyRequestHandlerTuple
import dispatch.implyRequestVerbs
import dispatch.url
import java.util.Date
import org.json4s.JsonAST._
import scalaz._
import Scalaz._

object DieStaemme {
  val dorfUebersicht = "overview_villages"

  private implicit def toSeq(a: (String, String)) = Seq(a)
  private val siteURL = ".die-staemme.de"
  private def main = host("www" + siteURL)

  private def index = main / "index.php"

  private def action(name: String) = ("action" -> name)

  private def login(userName: String) = index <<? action("login") << ("user" -> userName)

  private def loginReq(userName: String, password: String) = login(userName) <<? //add querry parameter ->
    Seq("server_list" -> "1", "show_server_selection" -> "1") << // add request attributs->
    Seq("password" -> password, "cookie" -> "false", "clear" -> "true")

  private def directLoginReq(userName: String, passwordHash: String, server: String) =
    login(userName) <<? ("server_" + server, "") << ("password" -> passwordHash)

  def getLoginData(user: String, pw: String) = {
    val r = Http(loginReq(user, pw) OK as.json4s.Json)
    for (JObject(fields) <- r) yield {
      fields(0) match {
        case JField("res", JString(o)) => parseLoginData(o).success
        case JField("error", JString(o)) => o.failure
      }
    }
  }

  private def parseLoginData(data: String) = {
    val root = HTML5Parser.loadXML(data)

    val server = (root \\ "div").filter(ns => (ns \ "@id").text equals "active_server").head \\ "a" \\ "@onclick"

    val pattern = "'server_(.*)'".r

    val worlds = server flatMap { s =>
      pattern findFirstIn (s.toString) map { w => val pattern(a) = w; a }
    }

    val inputs = root \\ "input"
    def filter(att: String) = inputs.filter(ns => (ns \ "@name").text equals att).head \ "@value" toString ()

    (filter("user"), filter("password"), worlds)
  }

  private def redirect(respons: Response): Future[(Response, Buffer[Cookie])] = {
    val cookies = Buffer.empty[Cookie]
    respons.getCookies().copyToBuffer(cookies)

    respons.getStatusCode() match {
      case 302 => {
        redirectRec(respons, cookies)
      }
      case _ => Future successful (respons, cookies)
    }
  }

  private def redirectRec(respons: Response, cookies: Buffer[Cookie]): Future[(Response, Buffer[Cookie])] = {
    respons.getCookies().copyToBuffer(cookies)
    respons.getStatusCode() match {
      case 302 => {
        val re = respons.getHeader("location")

        val request =
          if (re.startsWith("http"))
            url(re)
          else
            url(host(respons.getUri().getHost()).url + re)

        cookies foreach request.addCookie _
        Http(request).flatMap { a =>
          redirectRec(a, cookies)
        }
      }
      case _ => Future successful (respons, cookies)
    }
  }

  def login(user: UserLogin): Future[Session] = {
    val request = Http(directLoginReq(user.userName, user.passwordHash, user.world)) flatMap redirect _
    request map (respons => Session(user, respons._2))
  }

  case class Session(user: UserLogin, cookies: Buffer[Cookie]) {
    private def serverMain = host(user.world + siteURL)
    private def game = serverMain / "game.php"
    private def map = serverMain / "map.php"

    private def execute(requ: RequestBuilder) = {
      cookies foreach requ.addCookie _
      Http(requ) flatMap (r => redirectRec(r, cookies)) map (_._1)
    }

    def logout = execute(game <<? action("logout"))

    //screen & village
    val kartenScreen = "map"

    private def screenR(screen: String) = game <<? ("screen" -> screen)
    private def vilScreen(screen: String, id: Int) = screenR(screen) <<? "village" -> id.toString

    def screenRequest(screen: String) = execute(screenR(screen))

    def villagePage(v: Village, screen: String) = execute(vilScreen(screen, v.id))
    def villageOverview(v: Village) = execute(vilScreen("overview", v.id))

    //map stuff
    def querryMap(range: (Range, Range)) = execute {
      def mod(ra: Range) = {
        val sektorSize = 20
        (ra.start - ra.start % sektorSize) until (ra.lastElement + (sektorSize - ra.lastElement % sektorSize)) by 20
      }

      val sektors = for (x <- mod(range._1); y <- mod(range._2)) yield s"${x}_${y}" -> "1"

      map <<? Seq("v" -> "2", "e" -> new Date().getTime().toString) <<? sektors
    } map { r => JsonMapper.deserialize[List[JsonMapSektor]](r.getResponseBody()) }

    def querryMap(sektors: (Int, Int)*) = execute {

      val sektorsS = sektors map (s => s"${s._1 * 20}_${s._2 * 20}" -> "1")

      map <<? Seq("v" -> "2", "e" -> new Date().getTime().toString) <<? sektorsS
    } map { r => JsonMapper.deserialize[List[JsonMapSektor]](r.getResponseBody()) }

  }

}