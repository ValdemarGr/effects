package effects

import effects.Effect._
import implicits._
import scala.annotation.tailrec
import scala.annotation.nowarn
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Semaphore
import java.lang
import java.util.concurrent.atomic.AtomicReference

object RTS {
  def run[A](e: Effect[A]): A =
    e match {
      case Pure(x)           => x
      case SideEffect(eval)  => eval(())
      case Bind(fa, binding) => run(binding(run(fa)))
      case _                 => ???
    }

  sealed trait RunningProgram[A]
  final case class Done[A](a: A, remainingStackTime: Int) extends RunningProgram[A]
  final case class Exhausted[A](rest: Effect[A]) extends RunningProgram[A]
  final case class Yield[A](attempt: Unit => Option[Effect[A]], block: Unit => Effect[A]) extends RunningProgram[A]

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
            case _ => ???
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
                case _                           => ???
              }
            case (e @ Exhausted(_), d @ Done(_, _)) =>
              val effectA = runSome(d.remainingStackTime, e.rest)
              effectA match {
                case Done(a, remainingStackTime) => Done((d.a, a), remainingStackTime)
                case Exhausted(rest)             => Exhausted(rest.map(a => (d.a, a)))
                case _                           => ???
              }
            case (Exhausted(el), Exhausted(er)) => Exhausted[A](Parallel(el, er))
            case _                              => ???
          }
        case _ => ???
      }

  @tailrec
  def runConcurrent[A](e: Effect[A]): A =
    runSome(5, e) match {
      case Done(a, _)      => a
      case Exhausted(rest) => runConcurrent(rest)
      case _               => ???
    }

  trait ThreadPool {
    def schedule(program: Unit => Unit): Unit

    def stop(): Unit
  }

  object ThreadPool {
    def apply(size: Int) = {
      val current = new AtomicInteger(size)
      def getNext = current.getAndUpdate(i => i % size) % size

      def mkRunner: ((Unit => Unit) => Unit, Thread) = {
        println("creating runner")
        val sem = new Semaphore(0)
        val q = scala.collection.mutable.Queue[Unit => Unit]()

        def enqueue(task: Unit => Unit) = {
          synchronized(q.enqueue(task))
          sem.release(1)
        }

        println("defining thread")
        val t = new Thread {
          override def run =
            while (true) {
              sem.acquire(1) // await one queue'd element
              synchronized(q.removeHeadOption()) match {
                case None       => ??? // programming failure
                case Some(task) => task(()) // if there is a task, run it!
              }
            }
        }

        println("starting thread")
        t.start()

        (enqueue _, t)
      }
      val state = Array.fill(size)(mkRunner)

      new ThreadPool {
        def schedule(program: Unit => Unit): Unit = {
          val (enq, _) = state(getNext)
          enq(program)
        }

        @nowarn
        def stop() = state.foreach { case (_, t) => t.stop() }
      }
    }
  }

  def runPar[A](tp: ThreadPool)(n: Int, e: Effect[A]): RunningProgram[A] =
    if (n <= 0) Exhausted(e)
    else
      e match {
        case Pure(x)          => Done(x, n - 1)
        case SideEffect(eval) => Done(eval(()), n - 1)
        case Bind(fa, binding) =>
          runPar(tp)(n + 1, fa) match {
            case Exhausted(rest) => Exhausted(Bind(rest, binding))
            case Done(a, remainingStackTime) =>
              val bound = binding(a)
              runPar(tp)(remainingStackTime - 1, bound)
            case Yield(t, get) =>
              Yield(t.andThen(_.map(e => Bind(e, binding))), get.andThen(e => Bind(e, binding)))
          }
        case Parallel(fa, fb) =>
          val lar = new AtomicReference[Option[Any]](None)
          val mutex = new Semaphore(0)
          tp.schedule { _ =>
            val l = foldPar(tp)(fb)
            println(s"thread ${Thread.currentThread().getName()} got $l from $fb")
            lar.set(Some(l))
            mutex.release()
          }

          val r = foldPar(tp)(fa)
          // println(s"schedding $fb, $fb was $r")

          Yield(_ => lar.get().map(l => Pure((r, l))),
                _ => {
                  mutex.acquire()
                  Pure((r, lar.getAcquire().get))
                }
          )
        case Async(async) =>
          val ar = new AtomicReference[Option[A]](None)
          val mutex = new Semaphore(0)
          async { a =>
            ar.set(Some(a))
            mutex.release()
          }
          Yield(_ => ar.get().map(Pure(_)),
                _ => {
                  mutex.acquire()
                  Pure(ar.getAcquire().get)
                }
          )
      }

  @tailrec
  def foldPar[A](tp: ThreadPool)(e: Effect[A]): A =
    runPar(tp)(5, e) match {
      case Done(a, _)      => a
      case Exhausted(rest) => foldPar(tp)(rest)
      case Yield(_, get)   => foldPar(tp)(get(())) // not much more we can do, resume when async effect is done
    }

  def runParallel[A](e: Effect[A]): A = {
    val tp = ThreadPool(lang.Runtime.getRuntime().availableProcessors() * 2)

    val res = foldPar(tp)(e)
    tp.stop()
    res
  }
}
