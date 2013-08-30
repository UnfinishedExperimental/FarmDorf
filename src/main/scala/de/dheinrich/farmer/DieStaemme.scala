package de.dheinrich.farmer

import scala.Option.option2Iterable
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Buffer
import com.ning.http.client.Cookie
import com.ning.http.client.RequestBuilder
import com.ning.http.client.Response
import de.dheinrich.farmer.db.Village
import de.dheinrich.farmer.json.JsonMapper._
import de.dheinrich.farmer.json.JsonMapSektor
import dispatch.Defaults.executor
import dispatch.Future
import dispatch.as
import dispatch.host
import dispatch.implyRequestHandlerTuple
import dispatch.implyRequestVerbs
import dispatch.url
import java.util.Date
import org.json4s.JsonAST._
import scalaz._
import Scalaz._
import de.dheinrich.farmer.Report
import scala.concurrent._
import java.net.URL
import scala.annotation.tailrec
import dispatch.OkFunctionHandler
import com.ning.http.client.Request
import scala.xml.Elem
import scala.xml.Node

object DieStaemme {
  private val http = dispatch.Http.configure(_ setFollowRedirects true)

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
    val r = http(loginReq(user, pw) OK as.json4s.Json)
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
        dispatch.Http(request).flatMap { a =>
          redirectRec(a, cookies)
        }
      }
      case _ => Future successful (respons, cookies)
    }
  }

  def login(user: UserLogin): Future[Session] = {
    val request = dispatch.Http(directLoginReq(user.userName, user.passwordHash, user.world)) flatMap redirect _
    request map (respons => Session(user, respons._2))
  }

  case class Session(user: UserLogin, cookies: Buffer[Cookie]) {
    private def serverMain = host(user.world + siteURL)
    private def game = serverMain / "game.php"
    private def map = serverMain / "map.php"

    private def execute[T](requ: RequestBuilder, trans: Response => T = identity[Response] _) = {
      cookies foreach requ.addCookie _
      http(requ OK trans)
    }

    def logout = execute(game <<? action("logout"))

    private def screenR(screen: Screens.Value) = game <<? ("screen" -> screen.toString)
    private def vilScreen(screen: Screens.Value, id: Int) = screenR(screen) <<? "village" -> id.toString

    def screenRequest[A](screen: Screens.Value @@ A): Future[Node] @@ A = Tag(execute(screenR(screen), HTML5))

    private val pageItemCount = 12
    def reportOverview(page: Int): Future[Node] @@ ReportOverview = {
      val from = page * pageItemCount
      val req = game <<? Seq("screen" -> Screens.Report.toString, "mode" -> "attack", "from" -> from.toString)
      Tag(execute(req, HTML5))
    }

    def reportRequest(id: Int): Future[Node] @@ Report = {
      val req = game <<? Seq("screen" -> Screens.Report.toString, "view" -> id.toString)
      Tag(execute(req, HTML5))
    }

    def villagePage[A](v: Village, screen: Screens.Value @@ A): Future[Node] @@ A = Tag(execute(vilScreen(screen, v.id), HTML5))

    //map stuff
    val SEKTOR_SIZE = 20
    val MAX_SEKTORS_REQUEST = 100
    val WORLD_SIZE = 900

    def queryWorld() = {
      queryMap(0 until WORLD_SIZE, 0 until WORLD_SIZE)
    }

    def queryMap(xRange: Range, yRange: Range): Future[List[JsonMapSektor]] = {
      def mod(ra: Range) = {
        (ra.start - ra.start % SEKTOR_SIZE) until (ra.lastElement + (SEKTOR_SIZE - ra.lastElement % SEKTOR_SIZE)) by SEKTOR_SIZE
      }

      val sektors = for (x <- mod(xRange); y <- mod(yRange)) yield (x / SEKTOR_SIZE, y / SEKTOR_SIZE)
      queryMap(sektors)
    }

    private type jSektors = List[JsonMapSektor]

    private def now = new Date().getTime().toString

    @tailrec
    final def queryMap(sektors: Seq[(Int, Int)], already: Future[jSektors] = Future.successful(Nil)): Future[jSektors] = {
      val (thisReq, nextReq) = sektors.splitAt(MAX_SEKTORS_REQUEST)

      val querryAtt = thisReq map (s => (s._1 * SEKTOR_SIZE) + "_" + (s._2 * SEKTOR_SIZE) -> "1")
      val querry = map <<? Seq("v" -> "2", "e" -> now) <<? querryAtt
      val parsed = http(querry OK as.String) map deserialize[jSektors]

      val total = for (p <- parsed; a <- already) yield p ++ a

      if (nextReq.isEmpty)
        return total
      else
        queryMap(nextReq, total)
    }

  }

}


