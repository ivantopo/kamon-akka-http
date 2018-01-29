package akka.http.instrumentation

import java.util.concurrent.ConcurrentLinkedQueue

import akka.http.scaladsl.common.ToNameReceptacleEnhancements
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import akka.http.scaladsl.server.PathMatchers.IntNumber
import akka.http.scaladsl.server.directives.BasicDirectives.{extract, mapRequestContext, tprovide}
import akka.http.scaladsl.server.directives.PathDirectives
import akka.http.scaladsl.server.directives.RouteDirectives.reject
import akka.http.scaladsl.server._
import kamon.Kamon
import kamon.context.Key
import org.aspectj.lang.ProceedingJoinPoint
import org.aspectj.lang.annotation.{Around, Aspect, Before, DeclareMixin}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

@Aspect
class PathDirectivesInstrumentation {

  @Before("execution(* akka.http.scaladsl.server.RequestContextImpl.withUnmatchedPath(..)) && this(ctx) && args(path)")
  def beforeCopyingRequestContext(ctx: RequestContext, path: Uri.Path): Unit = {
    println("Copying Request Context")
    println("  Previous Path: " + ctx.unmatchedPath)
    println("  New Path     : " + path)
    //println("  Localtion: " + (new Throwable).getStackTraceString)
    println()
  }

  @Around("execution(* akka.http.scaladsl.server.PathMatcher+.apply(..)) && this(pm) && args(path)")
  def aroundPathMatcherApply(pjp: ProceedingJoinPoint, pm: PathMatcher[_], path: Uri.Path): Any = {
    import PathDirectivesInstrumentation._

    val matchedSegment: String = pm.getClass.getSimpleName match {
      case "Slash$"               => "/"
      case "IntNumber$"           => "IntNumber"
      case "$anon$1" | "$anon$3"  => ""
      case "anon$6"               => PrefixPathMatcherAccessor.get(pm).toString()
      case _                      => "unknown"
    }

    val pathMatchingContext = Kamon.currentContext().get(PathMatchingContext)
    pathMatchingContext.list = matchedSegment :: pathMatchingContext.list
    val result = pjp.proceed()

    if(result== Unmatched)
      pathMatchingContext.list = pathMatchingContext.list.tail

    result
  }


  @Around("execution(* akka.http.scaladsl.server.directives.PathDirectives.rawPathPrefix(..)) && args(pm)")
  def aroundRawPathPrefixDirective(pjp: ProceedingJoinPoint, pm: PathMatcher[_]): Any = {
    PathDirectivesInstrumentation.instrumentedRawPathPrefix(pm)
  }

  @DeclareMixin("akka.http.scaladsl.server.RequestContextImpl")
  def mixinInstrumentedRequestContext(): InstrumentedRequestContext = InstrumentedRequestContext()

  @Around("execution(* akka.http.scaladsl.server.RequestContextImpl.copy(..)) && this(oldContext)")
  def aroundCopyRequestContext(pjp: ProceedingJoinPoint, oldContext: InstrumentedRequestContext): Any = {
    println("COPYING A REQUEST CONTEXT")
    val newContext = pjp.proceed().asInstanceOf[InstrumentedRequestContext]
    newContext.addMatchedPath(oldContext.matchedPathTemplate())
    newContext
  }


}

trait InstrumentedRequestContext {
  def matchedPathTemplate(): String
  def addMatchedPath(matchedPath: String): Unit

  def hasCustomOperationName(): Boolean
  def hasCustomOperationName_=(hasCustomOperationName: Boolean): Unit

}

object InstrumentedRequestContext {

  def apply(): InstrumentedRequestContext = new InstrumentedRequestContext {
    @volatile private var _hasCustomOperationName: Boolean = false
    @volatile private var _matchedPathTemplate: String = ""

    override def addMatchedPath(matchedPath: String): Unit =
      _matchedPathTemplate = _matchedPathTemplate + matchedPath

    override def matchedPathTemplate(): String =
      _matchedPathTemplate

    override def hasCustomOperationName(): Boolean =
      _hasCustomOperationName

    override def hasCustomOperationName_=(hasCustomOperationName: Boolean): Unit =
      _hasCustomOperationName = hasCustomOperationName
  }
}



object PathDirectivesInstrumentation {
  case class MutableListWrapper(var list: List[String])

  val PathMatchingContext = Key.local[MutableListWrapper]("name-queue", MutableListWrapper(Nil))

  val PrefixPathMatcherAccessor = {
    val prefixField = Class.forName("akka.http.scaladsl.server.PathMatcher$$anon$6").getDeclaredField("prefix$1")
    prefixField.setAccessible(true)
    prefixField
  }

  def instrumentedRawPathPrefix[L](pm: PathMatcher[L]): Directive[L] = {
    implicit val LIsTuple = pm.ev
    extract(ctx ⇒ {
      val pathMatchingContext = MutableListWrapper(Nil)
      val pathMatcherResult = Kamon.withContextKey(PathMatchingContext, pathMatchingContext)(pm(ctx.unmatchedPath))

      if(pathMatcherResult.isInstanceOf[Matched[_]]) {
        val matchedPathTemplate = pathMatchingContext.list.reverse.mkString("")
        ctx.asInstanceOf[InstrumentedRequestContext].addMatchedPath(matchedPathTemplate)
        println("================> Finalized: " + matchedPathTemplate)
      }

      pathMatcherResult
    }).flatMap {
      case Matched(rest, values) ⇒ tprovide(values) & mapRequestContext(_ withUnmatchedPath rest)
      case Unmatched             ⇒ reject
    }
  }
}