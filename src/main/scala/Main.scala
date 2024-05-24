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
          case Left(_)  => ZIO.fail(new IOException("Failed to parse client balance"))
          case Right(v) => ZIO.succeed(v)
        }
      )

  def balancesToFile(fileName: String, balances: List[ClientBalanceRecord]) =
    val fileSink =
      ZSink
        .fromPath(Paths.get(fileName))
        .contramapChunks[String](_.flatMap(_.getBytes))

    ZStream
      .fromIterable(balances)
      .mapZIO(v =>
        ClientBalanceRecord.syntax.printString(v) match {
          case Left(_)  => ZIO.fail(new IOException("Failed to parse client balance"))
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
          case Left(_)  => ZIO.fail(new IOException("Failed to parse client order"))
          case Right(v) => ZIO.succeed(v)
        }
      )

  private def main: IO[Throwable, Unit] = {

    val output = runMatcher(clients, clientOrders)

    for {
      _ <- balancesFromFile("clients.txt").foreach(printLine(_))
      _ <- ordersFromFile("orders.txt").foreach(printLine(_))

      _ <- printLine(output)

      // TODO: write actual outputs to file
      stubOutputBalances <- balancesFromFile("clients.txt").run(
        ZSink.foldLeft(List.empty[ClientBalanceRecord])((b, a) => b ++ List(a))
      )
      _ <- balancesToFile("results.txt", stubOutputBalances)

    } yield ()
  }

  def run: IO[Throwable, Unit] = main
}

val clients = List(
  ClientBalanceRecord(
    ClientName("C1"),
    UsdAmount(1000).get,
    AssetAmount(10).get,
    AssetAmount(5).get,
    AssetAmount(15).get,
    AssetAmount(0).get
  ),
  ClientBalanceRecord(
    ClientName("C2"),
    UsdAmount(2000).get,
    AssetAmount(3).get,
    AssetAmount(35).get,
    AssetAmount(40).get,
    AssetAmount(10).get
  )
)

val clientOrders = List(
  ClientOrder
    .Buy(
      ClientName("C1"),
      AssetName("A"),
      UsdAmount(10).get,
      AssetPrice(12).get
    ),
  ClientOrder.Sell(
    ClientName("C2"),
    AssetName("A"),
    AssetAmount(8).get,
    AssetPrice(10).get
  )
)
