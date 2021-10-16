package effects

import effects.Effect._
import implicits._

object Runtime {
  def run[A](e: Effect[A]): A =
    e match {
      case Pure(x)           => x
      case SideEffect(eval)  => eval(())
      case Bind(fa, binding) => run(binding(run(fa)))
      case _: Parallel[_, _] => ???
    }

  sealed trait RunningProgram[A]
  final case class Done[A](a: A, remainingStackTime: Int) extends RunningProgram[A]
  final case class Exhausted[A](rest: Effect[A]) extends RunningProgram[A]

  def runSome[A](n: Int, e: Effect[A]): RunningProgram[A] =
    if (n <= 0) Exhausted(e)
    else
      e match {
        case Pure(x)          => Done(x, n - 1)
        case SideEffect(eval) => Done(eval(()), n - 1)
        case Bind(fa, binding) =>
          runSome(n + 1, fa) match {
            case Exhausted(rest) => Exhausted(Bind(rest, binding))
            case Done(a, remainingStackTime) =>
              val bound = binding(a)
              runSome(remainingStackTime - 1, bound)
          }
        case Parallel(fa, fb) =>
          // equally split the tokens
          val lToks = n / 2
          val rToks = n - lToks

          val l = runSome(lToks, fa)
          val r = runSome(rToks, fb)

          (l, r) match {
            case (Done(ra, rrtoks), Done(la, lrtoks)) =>
              Done((ra, la), rrtoks + lrtoks)
            case (d @ Done(_, _), e @ Exhausted(_)) =>
              val effectA = runSome(d.remainingStackTime, e.rest)
              effectA match {
                case Done(a, remainingStackTime) => Done((d.a, a), remainingStackTime)
                case Exhausted(rest)             => Exhausted(rest.map(a => (d.a, a)))
              }
            case (e @ Exhausted(_), d @ Done(_, _)) =>
              val effectA = runSome(d.remainingStackTime, e.rest)
              effectA match {
                case Done(a, remainingStackTime) => Done((d.a, a), remainingStackTime)
                case Exhausted(rest)             => Exhausted(rest.map(a => (d.a, a)))
              }
            case (Exhausted(el), Exhausted(er)) => Exhausted[A](Parallel(el, er))
          }
      }

  def runWithPar[A](e: Effect[A]): A =
    runSome(5, e) match {
      case Done(a, _)      => a
      case Exhausted(rest) => runWithPar(rest)
    }
}
