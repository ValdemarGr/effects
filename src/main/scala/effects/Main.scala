package effects

import effects.Effect._

object Main extends App {
  import implicits._
  def goN(name: String)(n: Int): Effect[Int] =
    if (n == 0) Pure(1)
    else Bind[Int, Unit](Library.putStrLn(s"$name: $n"), _ => goN(name)(n - 1))

  val fst = goN("first")(13)
  val snd = goN("second")(13)
  val sumManyParF = (1 to 5)
    .map(i => goN(s"program $i")(10))
    .foldLeft[Effect[Int]](Pure(0)) { case (accum, next) =>
      EffectT[Effect].par(accum, next).map { case (l, r) => l + r }
    }

  val program = for {
    str <- EffectT[Effect].pure("Hello world!")
    _ <- Library.putStrLn(str)
    (f, s) <- EffectT[Effect].par(fst, snd)
    _ <- Library.putStrLn(s"first $f, second $s")
    sum <- sumManyParF
    _ <- Library.putStrLn(s"sum is $sum")
  } yield ()

  RTS.runParallel(program)
  println("holla")
}
