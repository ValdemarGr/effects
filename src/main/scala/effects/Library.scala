package effects

object Library {
  def putStrLn(str: String): Effect[Unit] =
    Effect.SideEffect(_ => println(str))
}
