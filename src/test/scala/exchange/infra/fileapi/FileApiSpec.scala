package exchange.infra.fileapi

import exchange.domain.model.ExchangeState
import zio._
import zio.prelude._
import zio.stream._
import zio.test._

object FileApiSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] = suite("Simle cases")(
    test("Empty inputs produce empty outputs") {
      val expectedFileApiOutput = FileApiOutput(
        state = ExchangeState(balances = Map.empty, pendingOrders = List.empty),
        rejectedOrders = Vector.empty
      )
      for {
        out <- FileApi.runFromStrings(ZStream.empty, ZStream.empty).either
      } yield assertTrue(out === Right(expectedFileApiOutput))
    },
    test("Given input produces expected output") {
      // TODO: this test will fail when implementation added, but this is a reference test
      val clientBalances         = ZStream("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      val orders                 = ZStream("C1	b	A	10	12", "C2	s	A	8	10")
      val expectedOutputBalances = Set("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedOutputBalances))
      }
    },
    test("Duplicate client name leads to an error") {
      val clientBalances = ZStream("C1	1000	10	5	15	0", "C2	2000	3	35	40	10", "C1	500	3	35	40	10")
      val orders         = ZStream.empty
      val expectedError  = StringFileApiError.ItsFileApiError(FileApiError.ClientAlreadyExists)
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Left(expectedError))
      }

    }
  )

}
