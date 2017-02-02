package term

import scalaz._

object term {


    sealed trait TermF[A]

    object TermF {

        case class New(name: String, pi: TermF[Unit]) extends TermF[Unit]

        case class Parallel(p: TermF[Unit], q: TermF[Unit]) extends TermF[Unit]

        case class Receive(a: String, b: String, pi: TermF[Unit]) extends TermF[Unit]

        case class Send(a: String, b: String, pi: TermF[Unit]) extends TermF[Unit]

        case object Nil extends TermF[Unit]

    }

    type Term[A] = Free[Term, A]

    def new_(name: String, pi: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.New(name, pi))

    def parallel(p: TermF[Unit], q: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.Parallel(p, q))

    def receive(a: String, b: String, pi: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.Receive(a, b, pi))

    def send(a: String, b: String, pi: TermF[Unit]): Free[TermF, Unit] = Free.liftF(TermF.Send(a, b, pi))

    val nil: Free[TermF, Unit] = Free.liftF(TermF.Nil)


    val prog: Term[Unit] =
        for {
            _ <- nil
        } yield ()
}
