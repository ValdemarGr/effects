package effects

trait EffectT[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def pure[A](a: A): F[A]

  def par[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  def blocking[A](f: => A): F[A]
}

object EffectT {
  def apply[F[_]](implicit m: EffectT[F]): EffectT[F] = m
}
