import zio.test._

object MatcherSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Nothing] =
    suite("Simle cases")(
      test("Empty inputs produce empty outputs") {
        val actual = runMatcher(List.empty, List.empty)
        val expected = Right(
          MatcherOutput(
            state = MatcherState(balances = Map.empty, pendingOrders = List.empty),
            rejectedOrders = List.empty
          )
        )
        assertTrue(actual == expected)
      }
    )
}
