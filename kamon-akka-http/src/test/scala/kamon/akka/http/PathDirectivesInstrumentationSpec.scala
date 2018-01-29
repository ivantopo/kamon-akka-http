package kamon.akka.http

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import akka.http.scaladsl.server._
import Directives._
import akka.http.scaladsl.common.ToNameReceptacleEnhancements
import akka.http.scaladsl.model.Uri

class PathDirectivesInstrumentationSpec extends WordSpec with Matchers with ScalatestRouteTest {
//  val matcher: PathMatcher1[Option[Int]] =
//    "foo" / "bar" / "X" ~ IntNumber.? / ("edit" | "create")

  val route =
    get {
      pathPrefix("v1" / "users" / "42" / "others" / IntNumber | IntNumber) { i =>
        complete("ok")
      } ~
      pathPrefix("v2") {
        pathPrefix("usersnn") {
          complete("ok")
        } ~
        pathPrefix("us" ~ "ers" / IntNumber / "others" / IntNumber) { (n, m) =>
          complete("ok")
        }
      }
    }

  "The PathDirectives instrumentation" should {
    "match on stuff" in {
//      val path = Uri.Path("users/42/others/24")
//      val pathMatcher = PathMatcher("accounts" / IntNumber / IntNumber | "users" / IntNumber / "others" / IntNumber)
//
//      val x = pathMatcher(path)


      Get("/23") ~> route ~> check {
        responseAs[String] shouldEqual "ok"
      }
    }
  }

}
