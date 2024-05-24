import zio.test._
import zio.stream._

object MatcherSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Nothing] =
    suite("Simle cases")(
      test("Empty inputs produce empty outputs") {
        val expectedMatcherOutput = MatcherOutput(
          state = MatcherState(balances = Map.empty, pendingOrders = List.empty),
          rejectedOrders = List.empty
        )

        for {
          out <- runMatcher(ZStream.fromIterable(List.empty), ZStream.fromIterable(List.empty)).either
        } yield assertTrue(out == Right(expectedMatcherOutput))
      }
    )
}
