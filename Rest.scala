
load.ivy("org.scalaj" %% "scalaj-http" % "2.2.1")
load.ivy("org.json4s" %% "json4s-native" % "3.3.0")

@

import scalaj.http._
import scala.util.Try
import scala.text._
import org.json4s._
import org.json4s.native.JsonMethods._

var url = "https://httpbin.org"

implicit class StringHttp(path: String) {
  def get = {
    val fullUrl = if (path.matches("^https?://")) path else url + path
    Http(fullUrl).asString.body.parsed
  }
}

implicit class StringParse(str: String) {
  def parsed = parse(str)
}

repl.prompt.bind(Console.CYAN + url + Console.RESET + " | ")

implicit val ppJson = pprint.PPrinter[JValue] {
  case (jv: JValue, c: pprint.Config) =>

    def colorize(elem: Document): Document = {
      def color(c: String, str: String) = c + str + Console.RESET
      elem match {
        case DocText(t) if Seq("{", "}", "[", "]").contains(t)      => DocText(color(Console.MAGENTA, t))
        case DocText(t) if Seq("true", "false", "null").contains(t) => DocText(color(Console.YELLOW, t))
        case DocText(t) if t.startsWith("\"") && t.endsWith("\"")   => DocText(color(Console.GREEN, t))
        case DocText(t) if Try(t.toDouble).isSuccess                => DocText(color(Console.BLUE, t))
        case DocCons(h, t) => DocCons(colorize(h), colorize(t))
        case DocNest(i, d) => DocNest(i, colorize(d))
        case x => x
      }
    }

    pretty(colorize(render(jv)))
      .lines
      .map(_ + "\n")
      .toIterator
}

implicit val ppDyn = pprint.PPrinter[Dyn] {
  case (dyn: Dyn, c: pprint.Config) => ppJson.render(dyn.v, c)
}

import scala.language.dynamics

case class Dyn(v: JValue) extends Dynamic {
  def selectDynamic(field: String) = Dyn(v \ field)
}
