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
  private def main: IO[Throwable, Unit] = {
    val clientsStream = ZStream
      .fromFileName("clients.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

    val ordersStream = ZStream
      .fromFileName("orders.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
    val output = runMatcher(clients, clientOrders)

    def fileSink(path: Path): ZSink[Any, Throwable, String, Byte, Long] =
      ZSink
        .fromPath(path)
        .contramapChunks[String](_.flatMap(_.getBytes))

    val result = ZStream("Hello", "ZIO", "World!")
      .intersperse("\n")
      .run(fileSink(Paths.get("results.txt")))

    for {
      _ <- clientsStream.foreach(printLine(_))
      _ <- ordersStream.foreach(printLine(_))
      _ <- printLine(output)

      // TODO: write actual outputs to file
      _ <- ZStream
        .fromFileName("clients.txt")
        .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
        .intersperse("\n")
        .run(fileSink(Paths.get("results.txt")))
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
