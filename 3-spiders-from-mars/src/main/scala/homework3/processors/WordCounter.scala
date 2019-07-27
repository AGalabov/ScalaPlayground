package homework3.processors

import homework3.Processor
import homework3.html.HtmlUtils
import homework3.http.HttpResponse

import scala.concurrent.Future

case class WordCount(wordToCount: Map[String, Int])

object WordCount {
  def wordsOf(text: String): List[String] = text.split("\\W+").toList.filter(_.nonEmpty)
}

object WordCounter extends Processor[WordCount] {
  def getText(response: HttpResponse): String = response match {
    case _ if response.isSuccess && response.isPlainText => response.body
    case _ if response.isSuccess && response.isHtml => HtmlUtils.toText(response.body)
    case _ => ""
  }

  def apply(url: String, response: HttpResponse): Future[WordCount] =
    Future.
      successful(WordCount(WordCount.
        wordsOf(getText(response))
        .groupBy((word:String) => word)
        .mapValues(_.length)))
}

object NumberOfWords extends Processor[Int] {
  def apply(url: String, response: HttpResponse): Future[Int] = {
    Future.successful(WordCount.wordsOf(response.body).length)
  }
}
