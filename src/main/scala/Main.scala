import java.nio.file.Paths

import ClientName._
import zio._
import zio.stream._
import zio.prelude._

enum ApplicationError:
  case ItsMatcherError(e: MatcherError)
  case ItsPrinterError(e: String)
  case ItsOutputFileWriteError(e: Throwable)

def explainApplicationError(e: ApplicationError): String = e.toString() // TODO: implement

implicit val ApplicationErrorEqual: Equal[ApplicationError] =
  Equal.default

object Main extends ZIOAppDefault {

  def balancesFromFile(fileName: String) = ZStream
    .fromFileName(fileName)
    .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
    .mapZIO(s =>
      ClientBalanceRecord.syntax.parseString(s) match {
        case Left(_)  => ZIO.fail(new Exception("Failed to parse client balance"))
        case Right(v) => ZIO.succeed(v)
      }
    )

  def balancesToFile(fileName: String, balanceStrings: Set[String]) =
    val fileSink = ZSink
      .fromPath(Paths.get(fileName))
      .contramapChunks[String](_.flatMap(_.getBytes))

    ZStream
      .fromIterable(balanceStrings)
      .intersperse("\n")
      .run(fileSink)

  def ordersFromFile(fileName: String) = ZStream
    .fromFileName(fileName)
    .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
    .mapZIO(s =>
      clientOrderSyntax.parseString(s) match {
        case Left(_)  => ZIO.fail(new Exception("Failed to parse client order"))
        case Right(v) => ZIO.succeed(v)
      }
    )

  def main = {
    for {
      output <- runMatcher(balancesFromFile("clients.txt"), ordersFromFile("orders.txt"))
        .mapError(ApplicationError.ItsMatcherError(_))

      balanceStrings <- ZIO
        .fromEither(outputToClientBalanceStrings(output))
        .mapError(ApplicationError.ItsPrinterError(_))
      _ <- balancesToFile("results.txt", balanceStrings)
        .mapError(ApplicationError.ItsOutputFileWriteError(_))
    } yield ()
  }

  def run = main.mapError(e => new Exception(explainApplicationError(e))) // TODO: explain error

}
