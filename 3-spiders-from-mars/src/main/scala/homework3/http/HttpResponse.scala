package homework3.http

import java.nio.charset.{Charset, StandardCharsets}

import org.asynchttpclient.util.{HttpUtils => AsyncHttpUtils}

trait HttpResponse {
  def status: Int
  def headers: Map[String, String]
  def bodyAsBytes: Array[Byte]

  def body: String = {
    val charset = contentType.flatMap(_.charset).getOrElse(StandardCharsets.UTF_8)

    new java.lang.String(bodyAsBytes, charset)
  }

  def contentType: Option[ContentType] = headers.get("content-type").map { contentType =>
    val mimeType = contentType.split(";")(0).trim.toLowerCase
    val charset = Option(AsyncHttpUtils.extractContentTypeCharsetAttribute(contentType))

    ContentType(mimeType, charset)
  }

  def isHtml: Boolean = contentType match {
    case Some(t) if t.mimeType == ContentType.Html => true
    case _       => false
  }

  def isPlainText: Boolean = contentType match {
    case Some(t) if t.mimeType == ContentType.PlainText => true
    case _       => false
  }

  def isSuccess: Boolean = 200 <= status && status < 300
  def isClientError: Boolean = 400 <= status && status < 500
  def isServerError: Boolean = 500 <= status && status < 600
  def isBrokenLink: Boolean = status == 404
}

case class ContentType(mimeType: String, charset: Option[Charset])

object ContentType {
  val Html = "text/html"
  val PlainText = "text/plain"
}

object NullResponse extends HttpResponse {
  def status: Int = 200
  def headers: Map[String, String] = Map.empty[String,String]
  def bodyAsBytes: Array[Byte] = Array.emptyByteArray
}