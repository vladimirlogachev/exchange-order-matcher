import zio.Console.printLine

import zio.stream._
import java.io.IOException
import zio._
import java.nio.file.{Paths, Path}

import ClientName._
import AssetName._
import UsdAmount._
import AssetAmount._
import AssetPrice._

object Main extends ZIOAppDefault {
  def balancesFromFile(fileName: String) =
    ZStream
      .fromFileName(fileName)
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .mapZIO(s =>
        ClientBalanceRecord.syntax.parseString(s) match {
          case Left(_)  => ZIO.fail(new Exception("Failed to parse client balance"))
          case Right(v) => ZIO.succeed(v)
        }
      )

  def balancesToFile(fileName: String, balances: Iterable[ClientBalanceRecord]) =
    val fileSink =
      ZSink
        .fromPath(Paths.get(fileName))
        .contramapChunks[String](_.flatMap(_.getBytes))

    ZStream
      .fromIterable(balances)
      .mapZIO(v =>
        ClientBalanceRecord.syntax.printString(v) match {
          case Left(_)  => ZIO.fail(new Exception("Failed to print client balance"))
          case Right(s) => ZIO.succeed(s)
        }
      )
      .intersperse("\n")
      .run(fileSink)

  def ordersFromFile(fileName: String) =
    ZStream
      .fromFileName(fileName)
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .mapZIO(s =>
        clientOrderSyntax.parseString(s) match {
          case Left(_)  => ZIO.fail(new Exception("Failed to parse client order"))
          case Right(v) => ZIO.succeed(v)
        }
      )

  def run = {
    for {
      output <- runMatcher(balancesFromFile("clients.txt"), ordersFromFile("orders.txt")).mapError(e =>
        new Error(e.toString)
      )
      _ <- balancesToFile("results.txt", toFinalBalances(output.state))
    } yield ()
  }
}
