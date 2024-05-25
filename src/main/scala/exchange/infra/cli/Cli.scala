package exchange.infra.cli

import java.nio.file.Paths

import exchange.infra.fileapi._
import zio._
import zio.stream._

object Cli extends ZIOAppDefault:

  private def linesFromFile(fileName: String) =
    ZStream
      .fromFileName(fileName)
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  private def linesToFile(fileName: String, lines: Iterable[String]) =
    val fileSink = ZSink
      .fromPath(Paths.get(fileName))
      .contramapChunks[String](_.flatMap(_.getBytes))
    ZStream
      .fromIterable(lines)
      .intersperse("\n")
      .run(fileSink)

  /** Note: unless we have more IO error than streaming, `StringFileApiError` is enough
    */
  private def processFiles: IO[StringFileApiError, Unit] = for {
    balanceStrings <- FileApi.runFromStringsToStrings(linesFromFile("clients.txt"), linesFromFile("orders.txt"))
    _              <- linesToFile("results.txt", balanceStrings).mapError(StringFileApiError.ItsOtherStreamError(_))
  } yield ()

  def run: IO[Throwable, Unit] = for {
    _ <- Console.printLine("Starting...")
    _ <- processFiles.mapError(e => new Exception(explainStringFileApiError(e)))
    _ <- Console.printLine("Done!")
  } yield ()
