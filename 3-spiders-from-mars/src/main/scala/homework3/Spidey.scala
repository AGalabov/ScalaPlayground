package homework3

import homework3.html.HtmlUtils
import homework3.http._
import homework3.math.Monoid


import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

case class Level(responses: Map[String,HttpResponse])
case class Links(urls: List[String])

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {

  def getResource(url: String, retriesOnError: Int, tolerateErrors: Boolean): Future[HttpResponse] = {
    val response: Future[HttpResponse] = httpClient.get(url).recover {
      case _ if tolerateErrors => NullResponse
    }

    response.flatMap {
      case NullResponse if retriesOnError > 0 => getResource(url,retriesOnError - 1, tolerateErrors)
      case hR: HttpResponse if hR.isServerError && retriesOnError > 0 => getResource(url,retriesOnError - 1, tolerateErrors)
      case r => Future.successful(r)
    }
  }

  def processResponse[O : Monoid](url : String, response : Future[HttpResponse])(config : SpideyConfig)
                                 (processor: Processor[O]) : Future[O] =
    response
      .flatMap(value => processor.apply(url, value))
      .recover {
        case _ if config.tolerateErrors => Monoid[O].identity
      }

  def getLinks(url: String, response: Future[HttpResponse]): Future[List[String]] =
    response
        .map(http => {
        if (http.isHtml) {
          HtmlUtils.linksOf(http.body, url)
        } else {
          List.empty[String]
        }
      })

  def processAll[O: Monoid](urls: List[String], config: SpideyConfig)(processor: Processor[O]): Future[O] =
    Future
      .sequence(urls
        .map(url =>
          processResponse[O](url,getResource(url,config.retriesOnError,config.tolerateErrors))(config)(processor)))
      .map(list => list.foldRight(Monoid[O].identity)(Monoid[O].op))

  def processLevel[O:Monoid](urls: Future[List[String]], config: SpideyConfig)
                            (processor: Processor[O]): Future[O] =
    urls.flatMap(processAll(_,config)(processor))

  def getAllNext(urls: List[String], config: SpideyConfig): Future[List[String]] =
    Future.sequence(
      urls.map(url => getLinks(url,getResource(url,config.retriesOnError,config.tolerateErrors))))
      .map(_.flatten)

  def getNextLevel[O:Monoid](urls: Future[List[String]], visited: Future[List[String]], config: SpideyConfig): Future[List[String]] =
    urls
      .flatMap(getAllNext(_,config))
      .map(list =>
        list
          .filter(url => !config.sameDomainOnly || HttpUtils.sameDomain(url,url))
          .distinct
          .filter(HttpUtils.isValidHttp))
        .flatMap(list =>
        visited.map(v => list.filterNot(url => v.contains(url))))

  def shutdown(): Unit = httpClient.shutdown()

  def crawlSpidey[O: Monoid](url: String, currentLevel: Future[List[String]], visited: Future[List[String]], config: SpideyConfig)
                            (processor: Processor[O]) : Future[O] = {
    if(config.maxDepth > 0) {
      val newConfig = config.copy(maxDepth = config.maxDepth - 1)
      val nextLevel = getNextLevel(currentLevel,visited,config)
      val newVisited: Future[List[String]] = visited.flatMap(v => nextLevel.map(v ::: _))
      processLevel(currentLevel,config)(processor)
          .flatMap(v =>
            crawlSpidey(url,nextLevel,newVisited,newConfig)(processor)
            .map(Monoid[O].op(v,_)))
    } else {
      processLevel(currentLevel,config)(processor)
    }
  }

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]) : Future[O] = {
    val first = Future.successful(List(url))
    val result = crawlSpidey[O](url,first,first,config)(processor)
    result onComplete {
      _ => shutdown()
    }
    result
  }

  def print[O : Monoid](result : Future[O]) : Unit =
    result onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println("Fail: " + exception)
  }
}