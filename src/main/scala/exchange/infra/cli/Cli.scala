package exchange.infra.cli

import java.nio.file.Paths

import zio._
import zio.stream._

import exchange.infra.fileapi._

object Cli extends ZIOAppDefault:

  /** Reads lines from a file, splits by newline chars.
    */
  private def linesFromFile(fileName: String) =
    ZStream
      .fromFileName(fileName)
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  /** Writes lines to a file, separeted by newline chars.
    */
  private def linesToFile(fileName: String, lines: Iterable[String]) =
    val fileSink = ZSink
      .fromPath(Paths.get(fileName))
      .contramapChunks[String](_.flatMap(_.getBytes))
    ZStream
      .fromIterable(lines)
      .intersperse("\n")
      .run(fileSink)

  /** Reads files, processes them and writes the result.
    *
    * Note: unless we have other IO errors than from file streaming, `StringFileApiError` is enough.
    */
  private def processFiles: IO[StringFileApiError, Unit] = for {
    balanceStrings <- FileApi.runFromStringsToStrings(linesFromFile("clients.txt"), linesFromFile("orders.txt"))
    _              <- linesToFile("results.txt", balanceStrings).mapError(StringFileApiError.ItsOtherStreamError(_))
  } yield ()

  def run: UIO[Unit] = processFiles
    .tapBoth(
      e =>
        Console.printLine(
          s"""|Error:
              |${explainStringFileApiError(e)}""".stripMargin
        ),
      _ => Console.printLine("Done!")
    )
    .exitCode
    .flatMap(exit)
