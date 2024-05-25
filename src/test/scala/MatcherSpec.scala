import zio._
import zio.prelude._
import zio.stream._
import zio.test._

import exchange.domain.model.MatcherState

/** Unit testing helper
  */
def balancesFromString(s: String) = ZStream
  .fromIterable(s.split("\n"))
  .mapZIO(s =>
    clientBalanceRecordSyntax.parseString(s) match {
      case Left(_)  => ZIO.fail(new Exception("Failed to parse client balance"))
      case Right(v) => ZIO.succeed(v)
    }
  )

/** Unit testing helper
  */
def ordersFromString(s: String) = ZStream
  .fromIterable(s.split("\n"))
  .mapZIO(s =>
    clientOrderSyntax.parseString(s) match {
      case Left(_)  => ZIO.fail(new Exception("Failed to parse client order"))
      case Right(v) => ZIO.succeed(v)
    }
  )

/** TODO: rewrite
  */
def balancesToString(balances: Set[ClientBalanceRecord]) = balances
  .map(clientBalanceRecordSyntax.printString(_))
  .foldRight(Right(""): Either[String, String])((aE, accE) => accE.flatMap(acc => aE.map(a => a ++ "\n" ++ acc)))
  .getOrElse("")

object MatcherSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] = suite("Simle cases")(
    test("Empty inputs produce empty outputs") {
      val expectedFileApiOutput = FileApiOutput(
        state = MatcherState(balances = Map.empty, pendingOrders = List.empty),
        rejectedOrders = List.empty
      )

      for {
        out <- runMatcher(ZStream.fromIterable(List.empty), ZStream.fromIterable(List.empty)).either
      } yield assertTrue(out === Right(expectedFileApiOutput))
    },
    test("Given input produces expected output") {
      val balancesStream = balancesFromString(
        """C1	1000	10	5	15	0
          |C2	2000	3	35	40	10
          |""".stripMargin
      )
      val ordersStream = ordersFromString(
        """C1	b	A	10	12
          |C2	s	A	8	10
          |""".stripMargin
      )
      val expectedOutputBalances = """C1	1000	10	5	15	0
                                     |C2	2000	3	35	40	10
                                     |""".stripMargin
      for {
        outputEither <- runMatcher(balancesStream, ordersStream).either
      } yield {
        val strBalances = outputEither.map(out => balancesToString(toFinalBalances(out.state)))
        assertTrue(strBalances === Right(expectedOutputBalances))
      }
    },
    test("Duplicate client name leads to an error") {
      val balancesStream = balancesFromString(
        """C1	1000	10	5	15	0
          |C2	2000	3	35	40	10
          |C1	500	3	35	40	10
          |""".stripMargin
      )
      val ordersStream  = ordersFromString("")
      val expectedError = FileApiError.ItsClientLoadError(ClientLoadError.ClientAlreadyExists)
      for {
        outputEither <- runMatcher(balancesStream, ordersStream).either
      } yield {
        assertTrue(outputEither === Left(expectedError))
      }
    }
  )

}
