package me.chuwy.otusbats

import scala.util.Try

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
}

object Monad {

  def apply[F[_]]()(implicit env: Monad[F]): Monad[F] = env

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = flatMap(fa)(a => point(f(a)))

  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = a :: Nil

    override def map[A, B](fa: List[A])(f: A => B): List[B] = flatMap(fa)(a => point(f(a)))
  }

  implicit val tryMonad: Monad[Try] = new Monad[Try] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def point[A](a: A): Try[A] = Try(a)

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = flatMap(fa)(a => point(f(a)))
  }


}
