package homework3.processors

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

import homework3.Processor
import homework3.http.HttpResponse

import scala.concurrent.{ExecutionContext, Future}

case class SavedFiles(urlToPath: Map[String, Path])

class FileOutput(targetDir: String)
                (ex: ExecutionContext) extends Processor[SavedFiles] {
  private implicit val blockingExc: ExecutionContext = ex

  private val targetPath = Paths.get(targetDir)

  private def generatePathFor(url: String): Path = {
    val urlFileName = Option(Paths.get(new URI(url).getPath).getFileName).map(_.toString).getOrElse("")
    val fileName = s"${UUID.randomUUID().toString}-$urlFileName"
    targetPath.resolve(fileName)
  }

  def apply(url: String, response: HttpResponse): Future[SavedFiles] = {
    val path = generatePathFor(url)
    val future = Future(Files.write(path, response.bodyAsBytes))
    future.map(_ => SavedFiles(Map(url -> path)))
  }
}
