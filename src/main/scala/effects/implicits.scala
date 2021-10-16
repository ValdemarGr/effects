package effects

import effects.Effect._

object implicits {
  implicit val effectEffectT = new EffectT[Effect] {
    def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] = Effect.Bind(fa, f)

    def map[A, B](fa: Effect[A])(f: A => B): Effect[B] =
      fa match {
        case Pure(x)          => Pure(f(x))
        case SideEffect(eval) => SideEffect(_ => f(eval(())))
        case Bind(fa, binding) => // rank-2 types please
          val g: Any => Effect[B] = (a: Any) => map(binding(a))(f)
          Bind[B, Any](fa, g)
        case Parallel(fa, fb) =>
          val g: ((Any, Any)) => Effect[B] = { case (l, r) => Pure(f((l, r).asInstanceOf[A])) }
          val e: Effect[(Any, Any)] = Parallel(fa, fb)
          Bind[B, (Any, Any)](e, g)
        case Async(async) => Async[B](complete => async(a => complete(f(a))))
      }

    def pure[A](a: A): Effect[A] =
      Pure(a)

    def par[A, B](fa: Effect[A], fb: Effect[B]): Effect[(A, B)] = Parallel(fa, fb)

    def blocking[A](f: => A): Effect[A] = Async[A](complete => complete(f))
  }

  implicit class EffectTOps[F[_]: EffectT, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = EffectT[F].map(fa)(f)

    def flatMap[B](f: A => F[B]): F[B] = EffectT[F].flatMap(fa)(f)

    def pure(a: A): F[A] = EffectT[F].pure(a)
  }
}
