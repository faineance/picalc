package term

import scala.collection.immutable
import cats.data.State
import cats.free.Free
import cats.{Id, ~>}

object term {

    sealed trait TermF[A]

    object TermF {

        case class New(name: String, pi: TermF[Unit]) extends TermF[Unit]

        case class Parallel(p: TermF[Unit], q: TermF[Unit]) extends TermF[Unit]

        case class Receive(a: String, b: String, pi: TermF[Unit]) extends TermF[Unit]

        case class Send(a: String, b: String, pi: TermF[Unit]) extends TermF[Unit]

        case object Nil extends TermF[Unit]

    }


    // Free monad over the free functor of TermF
    type Term[A] = Free[TermF, A]

    // constructors
    def new_(name: String, pi: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.New(name, pi))

    def parallel(p: TermF[Unit], q: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.Parallel(p, q))

    def receive(a: String, b: String, pi: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.Receive(a, b, pi))

    def send(a: String, b: String, pi: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.Send(a, b, pi))

    val nil: Free[TermF, Unit] = Free.liftF(TermF.Nil)

    type Env[A] = State[Map[String, Any], A]


    val eval: TermF ~> Env = new (TermF ~> Env) {
        def apply[A](fa: TermF[A]): Env[A] =
            fa match {

                case TermF.New(name, pi) =>

                    State.modify(_.updated(name, pi))
                case TermF.Parallel(p, q) => ???
                case TermF.Receive(a, b, pi) => ???
                case TermF.Send(a, b, pi) => ???
                case TermF.Nil => ???
                    
            }
    }

    val example: Term[Unit] =
        for {
            _ <- nil
        } yield ()

    val test: (Map[String, Any], Unit) = example.foldMap(eval).run(Map.empty).value
}

