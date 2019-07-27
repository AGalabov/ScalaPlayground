package homework3

import java.io.File

import scala.concurrent.{ExecutionContext}
import org.scalatest.{AsyncFlatSpec, Matchers}

import homework3.http.{HttpResponse}
import homework3.processors._


class ProcessorTest extends AsyncFlatSpec with Matchers {

  val httpResponse = new HttpResponse {
    override def status: Int = 200
    override def headers: Map[String, String] = Map("content-type" -> "text/html; charset=utf-8")
    override def bodyAsBytes: Array[Byte] = "Hello world. Hello friends".getBytes()
  }

  val mockUrl= "https://www.test.fmi.net"

  "BrokenLinkDetector.apply" should "return empty Set for successfult HttpResponse" in {
    val result = BrokenLinkDetector.apply(mockUrl,httpResponse)
    result.map(_ shouldBe Set.empty[String])
  }

  it should "return Set with 1 element for HttpResponse with code  404" in {
    val httpResponseBroken = new HttpResponse {
      override def status: Int = 404
      override def headers: Map[String, String] = Map.empty[String,String]
      override def bodyAsBytes: Array[Byte] = "Empty".getBytes()
    }

    val result = BrokenLinkDetector.apply(mockUrl,httpResponseBroken)
    result.map(_ shouldBe Set(mockUrl))
  }

  "WordCounter.apply" should "return number of occurances of each word in the HttpResponse" in {
    val result = WordCounter.apply(mockUrl,httpResponse)
    val expectedResult = WordCount(Map("Hello" -> 2, "world" -> 1, "friends" -> 1))
    result.map(_ shouldBe expectedResult)
  }

  it should "create a new file" in {
    val dir = "./testFiles"
    val output = new FileOutput(dir)(ExecutionContext.fromExecutor(
      new java.util.concurrent.ForkJoinPool(16)
    ))
    val result = output.apply(mockUrl,httpResponse)
    result.map(res => {
      val path = res.urlToPath.get(mockUrl).getOrElse("").toString
      val fileOutput = scala.io.Source.fromFile(path).mkString
      new File(path).delete()
      fileOutput shouldBe "Hello world. Hello friends"
    })
  }

}




