package effects

sealed trait Effect[+A]

object Effect {
  final case class Pure[A](x: A) extends Effect[A]
  final case class SideEffect[A](eval: Unit => A) extends Effect[A]
  final case class Bind[A, B](fa: Effect[B], binding: B => Effect[A]) extends Effect[A]
  final case class Parallel[A, B](fa: Effect[A], fb: Effect[B]) extends Effect[(A, B)]
  final case class Async[A, B](async: (Either[Throwable, String] => Unit) => Unit) extends Effect[(A, B)]
}
