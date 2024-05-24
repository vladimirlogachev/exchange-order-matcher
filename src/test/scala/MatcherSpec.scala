import zio._
import zio.prelude._
import zio.stream._
import zio.test._

def balancesFromString(s: String) = ZStream
  .fromIterable(s.split("\n"))
  .mapZIO(s =>
    ClientBalanceRecord.syntax.parseString(s) match {
      case Left(_)  => ZIO.fail(new Exception("Failed to parse client balance"))
      case Right(v) => ZIO.succeed(v)
    }
  )

def balancesToString(balances: Iterable[ClientBalanceRecord]) = balances
  .map(ClientBalanceRecord.syntax.printString(_))
  .foldRight(Right(""): Either[String, String])((aE, accE) => accE.flatMap(acc => aE.map(a => a ++ "\n" ++ acc)))

def ordersFromString(s: String) = ZStream
  .fromIterable(s.split("\n"))
  .mapZIO(s =>
    clientOrderSyntax.parseString(s) match {
      case Left(_)  => ZIO.fail(new Exception("Failed to parse client order"))
      case Right(v) => ZIO.succeed(v)
    }
  )

object MatcherSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] = suite("Simle cases")(
    test("Empty inputs produce empty outputs") {
      val expectedMatcherOutput = MatcherOutput(
        state = MatcherState(balances = Map.empty, pendingOrders = List.empty),
        rejectedOrders = List.empty
      )

      for {
        out <- runMatcher(ZStream.fromIterable(List.empty), ZStream.fromIterable(List.empty)).either
      } yield assertTrue(out === Right(expectedMatcherOutput))
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
        outEither <- runMatcher(balancesStream, ordersStream).either
      } yield {
        val strBalances = outEither.flatMap(out => balancesToString(toFinalBalances(out.state))).left.map(_.toString)
        assertTrue(strBalances === Right(expectedOutputBalances))
      }
    }
  )

}
