package kontur.scalatestrefactor

import scalaz._
import Scalaz._

trait FillMinimal {
  self : ExpertPage =>
  def fillMinimal : self.type = this
}

//next must pass state to a next page
//thus, it must know about the fact, that state is present
//thus, we need a constructable to be able to pass a state
//and state by itself should be constructable

trait ExpertPage extends FillMinimal

//generic traits
trait Construct[A] {
  def get : A
}

//for higher-kinded context bounds
// object HKHelper {
//   type HKConstruct[A[_]] = Construct[A[_]]
// }

trait WizardNextStep[Current <: ExpertPage, Next <: ExpertPage] extends WizardNextStepOps[Current, Next]{
  self : ExpertPage =>
  def next : Next
}

// object WizardNextStepOps {
// //   implicit def toWizardNextStepOps[Result <: ExpertPage, Next <: ExpertPage, A[_ <: ExpertPage] <: ExpertPage](x : A[Result])
// //               (implicit ev : A[Result] => WizardNextStep[A[Result], Next]) : WizardNextStepOps[A[Result], Next] =
// //     new WizardNextStepOps[A[Result], Next] {
// //       def skip = x.fillMinimal.next
// //     }
//   // implicit def toWizardNextStepOps[Result <: ExpertPage, Next <: ExpertPage, A[_ <: ExpertPage] <% WizardNextStep[A[_ <: ExpertPage], Next]](x : A[Result])
//   //             : WizardNextStepOps[A[Result], Next] =
//   //   new WizardNextStepOps[A[Result], Next] {
//   //     def skip = x.fillMinimal.next
//   //   }
// }

trait WizardNextStepOps[Current <: ExpertPage, Next <: ExpertPage] {
  self : WizardNextStep[Current, Next] with ExpertPage =>
  def skip : Next = self.fillMinimal.next
}



trait WizardPrevStep[Current <: ExpertPage, Prev <: ExpertPage] {
  def prev : Prev
}

class Connector[T : Manifest] {
  implicit val ev = implicitly[Manifest[T]]
  type Tp = T

  def getImplicitConstructorIfAvailable[T](implicit constructable : Construct[T] = null) =
    if (constructable != null) Some(constructable) else None

  def construction : Unit=>T = (Unit : Unit) => {
    println("not overriden")
    getImplicitConstructorIfAvailable[Tp] match {
      case Some(constructable) => {
        println("invoking constructable")
        val r = constructable.get
        println("got after construct: " + r.toString())
        r
      }
      case None =>
        throw new RuntimeException("Cannot locate implicit constructable, " +
            "you must either make implicit available or override Connector.construction")
    }
  }
}


//TODO: skipper

object Ops {
  def skip[A <: ExpertPage, B <: ExpertPage](a : A)(implicit ev : A => WizardNextStep[A, B]) : B =
    a.fillMinimal.next
}

//pages predefined
class Page1[Type <: ExpertPage] extends ExpertPage

object Page1 {
  //pages to construct
  //we can use reflection or macros to autogenerate it
  implicit def page1ToConstruct[A <: ExpertPage]: Construct[Page1[A]] =
    new Page1 with Construct[Page1[A]] {
      def get = new Page1[A]
    }

  //pages to steps
  //we can use macros to autogenerate it
  implicit def page1ToNextStep[A <: ExpertPage](x : Page1[A]): WizardNextStep[Page1[A], Page2[A]] =
    new Page1 with WizardNextStep[Page1[A], Page2[A]] {
      def next = implicitly[Construct[Page2[A]]].get
    }
}

class Page2[Type <: ExpertPage] extends ExpertPage

object Page2 {
  implicit def page2ToConstruct[A <: ExpertPage]: Construct[Page2[A]] =
    new Page2 with Construct[Page2[A]] {
      def get = new Page2[A]
    }


  implicit def page2ToNextStep[A <: ExpertPage](x : Page2[A]): WizardNextStep[Page2[A], CPage[Page3[A], A]] =
    new Page2 with WizardNextStep[Page2[A], CPage[Page3[A], A]] {
      def next = implicitly[Construct[CPage[Page3[A], A]]].get
    }

  implicit def page2ToPrevStep[A <: ExpertPage](x : Page2[A]): WizardPrevStep[Page2[A], Page1[A]] =
    new Page2 with WizardPrevStep[Page2[A], Page1[A]] {
      def prev = implicitly[Construct[Page1[A]]].get
    }
}

class Page3[Type <: ExpertPage] extends ExpertPage

object Page3 {
  implicit def page3ToConstruct[A <: ExpertPage]: Construct[Page3[A]] =
    new Page3 with Construct[Page3[A]] {
      def get = new Page3[A]
    }

  implicit def page3ToPrevStep[A <: ExpertPage](x : Page3[A]): WizardPrevStep[Page3[A], Page2[A]] =
    new Page3 with WizardPrevStep[Page3[A], Page2[A]] {
      def prev = implicitly[Construct[Page2[A]]].get
    }

  implicit def page3ToNextStep[A <: ExpertPage : Construct](x : Page3[A]): WizardNextStep[Page3[A], A] =
    new Page3 with WizardNextStep[Page3[A], A] {
      def next = implicitly[Construct[A]].get
    }
}

//this page can have several next pages
class CPage[Next <: ExpertPage, Type <: ExpertPage] extends ExpertPage {
  def check2010 = new CPage[Page2[Type], Type]
}

object CPage {
  def apply[A <: ExpertPage]() = new CPage[Page3[A], A]

  implicit def cPageToConstruct[B <: ExpertPage, A[B <: ExpertPage] <: ExpertPage]: Construct[CPage[A[B], B]] =
    new CPage with Construct[CPage[A[B], B]] {
      def get = new CPage[A[B], B]
    }

  // implicit def CPageToNextStep[A <: ExpertPage, Next[_] <: ExpertPage : ({type λ[X[A]] = Construct[X[A]]})#λ](x : CPage[Next[A], A])
  //   : WizardNextStep[CPage[Next[A], A], Next[A]] =
  //   new CPage[Next[A], A] with WizardNextStep[CPage[Next[A], A], Next[A]] {
  //     def next = implicitly[Construct[Next[A]]].get
  //   }

  //IMPORTANT: MUST PROVIDE BOUND ON _ IN NEXT[_]
  //Also, we cannot use context bounds here: see http://yz.mit.edu/wp/true-scala-complexity/ (
  //"we have to take the converter in as an “explicit” implicit parameter in a curried argument
  //list such that the type inference can flow from the first argument list to the type parameter list to the second argument list")
  implicit def CPageToNextStep[A <: ExpertPage, Next[_ <: ExpertPage] <: ExpertPage](x : CPage[Next[A], A])(implicit ev : Construct[Next[A]])
    : WizardNextStep[CPage[Next[A], A], Next[A]] =
    new CPage[Next[A], A] with WizardNextStep[CPage[Next[A], A], Next[A]] {
      def next = ev.get
    }

  implicit def CPageToPrevStep[A <: ExpertPage, Next[_ <: ExpertPage] <: ExpertPage](x : CPage[Next[A], A]) //(implicit ev : Construct[Next[A]])
    : WizardPrevStep[CPage[Next[A], A], Page3[A]] =
    new CPage[Next[A], A] with WizardPrevStep[CPage[Next[A], A], Page3[A]] {
      def prev = new Page3[A]
    }
}

class FaPage extends ExpertPage

object FaPage {
  implicit def faPageToConstruct: Construct[FaPage] =
    new FaPage with Construct[FaPage] {
      def get = new FaPage
  }

  implicit def faPageToPrevStep(x : FaPage): WizardPrevStep[FaPage, Page3[FaPage]] =
    new FaPage with WizardPrevStep[FaPage, Page3[FaPage]] {
      def prev = implicitly[Construct[Page3[FaPage]]].get
    }
}

class CreditPage extends ExpertPage

object CreditPage {
  implicit def creditPageToConstruct: Construct[CreditPage] =
    new CreditPage with Construct[CreditPage] {
      def get = new CreditPage
  }
}

trait StateWrapper[T <: ExpertPage] {
  val x : T
}

object StateWrapper {
  implicit def toState[T <: ExpertPage](x : StateWrapper[T]) : State[StateWrapper[T], StateWrapper[T]] =
    state[StateWrapper[T], StateWrapper[T]](x)
}

object ToState {
  implicit def PageToState[T <: ExpertPage](y : T) = new StateWrapper[T] {
    val x = y
  }

  def goFurther[T <: ExpertPage, U <: ExpertPage](implicit ev : T => WizardNextStep[T, U]) =
    State[StateWrapper[_ <: ExpertPage], U] {
      case x => {
        val n = ev(x.x.asInstanceOf[T]).next;
        (new StateWrapper[U] { val x = n }, n)
      }
    }

  def test = {
    val start =  PageToState(new Page1)
    for {
      x <- StateWrapper.toState(start)
      y <- goFurther
      z <- goFurther
    } yield (z)
  }
}

//something like final:
// scala> def pop[X] = State[List[X], X] {
//      | case x :: xs => (xs, x)
//      | }
// <console>:16: warning: match may not be exhaustive.
// It would fail on the following input: Nil
//        def pop[X] = State[List[X], X] {
//                                       ^
// pop: [X]=> scalaz.State[List[X],X]

// scala> pop(List(1, 2, 3))
// res0: (List[Int], Int) = (List(2, 3),1)

// scala> def push[X](a : X) = State[List[X], Unit] {
//      | case xs => (a :: xs, ())
//      | }
// push: [X](a: X)scalaz.State[List[X],Unit]

// scala> def stackAct : State[List[Int], Int] = for {
//      | _ <- push(3)
//      | a <- pop
//      | b <- pop
//      | } yield (b + a)
// stackAct: scalaz.State[List[Int],Int]

// scala> stackAct(List(1, 2, 3, 4, 5))
// res1: (List[Int], Int) = (List(2, 3, 4, 5),4)


object SSSTate {
  implicit def toStateM[S, A](g : S => (S, A)): State[S, A] = State(g)

  def goFurther[T <: ExpertPage, U <: ExpertPage](implicit ev : T => WizardNextStep[T, U]) =
    {
      val s = for (s <- get[T]) yield s
      val n = ev(s.asInstanceOf[T]).next
      (n, n)
    }

  // lazy val test =
  // for {
  //   x <- state[Page1, Page1](new Page1)
  //   y <- goFurther
  // } yield (y)
}


//  def goFurther[T <: ExpertPage, U <: ExpertPage](implicit ev : T => WizardNextStep[T, U]) = State[StateWrapper[_ <: ExpertPage], U]{
//    case x => { val n = ev(x.x.asInstanceOf[T]).next; (new StateWrapper[U] { val x = n }, n) }}
// goFurther: [T <: kontur.scalatestrefactor.ExpertPage, U <: kontur.scalatestrefactor.ExpertPage](implicit ev: T => kontur.scalatestrefactor.WizardNextStep[T,U])scalaz.IndexedStateT[scalaz.Id.Id,StateWrapper[_ <: kontur.scalatestrefactor.ExpertPage],StateWrapper[_ <: kontur.scalatestrefactor.ExpertPage],U]


// trait WizardStep[Prev <: ExpertPage, Current <: ExpertPage, Next<: ExpertPage]
//       extends WizardStepOps[Prev, Current, Next] {
//   def next : Next
//   def prev : Prev
// }

//we can extend WizardStep
// trait WizardStepOps[Prev <: ExpertPage, Current <: ExpertPage, Next <: ExpertPage] {
//   self : WizardStep[Prev, Current, Next] =>

//   def proceed : Next = next
// }

// object MonadicTester {
//   //try to devise underlying type with following props:
//   // - knows who is current, next and prev (if available)
//   // - makes all of them implicitly available
//   // - knows if page is present

//   def startWith = state[Option[Page1], Page1](new Page1)
//   def goFurther[Prev <: ExpertPage, Current <: ExpertPage <% WizardStep[Prev, Current, Next], Next <: ExpertPage] =
//     State[Option[Current], Next] { case old@Some(x) =>
//       val next = x.proceed
//       (old, next)
//     }

//   for {
//     x <- startWith
//     _ <- goFurther[Page1, Page1, Page2] //how to drop explicit arguments?
//   } yield(x)
// }

// object MonadicTester2 {
//   trait Underlying {
//     type Current <: ExpertPage
//   }
//   case class InWizard[Prev <: ExpertPage, Next <: ExpertPage](curr : Underlying#Current)
//   					(implicit ev : Underlying#Current =>
//   					   WizardStep[Prev, Underlying#Current, Next])
//   	   extends Underlying {
//     type P = Prev
//     type N = Next
//     def asWizard = ev(curr)
//   }
//   case class NotInWizard() extends Underlying {
//     type Page <: Option[ExpertPage]
//   }

//   def startWith = state[Underlying, Page1](new Page1)
//   def goFurther[A <: ExpertPage, B <: ExpertPage] =
//     State[Underlying, InWizard[A, B]#N] { case old@InWizard(x) =>
//       val next = old.asWizard.proceed
//       (old, next.asInstanceOf[B])
//     }

//   val fin =
//     for {
//       x <- startWith
//       _ <- goFurther[Page1, Page2] //how to drop explicit arguments?
//       fin <- goFurther[Page2, Page3]
// 	} yield(fin)
// }



object App {
  def main(args: Array[String]) {
//     scala> (new Page1[FaPage]).next.next.next.prev.prev
// res0: kontur.scalatestrefactor.Page1[kontur.scalatestrefactor.FaPage] = kontur.scalatestrefactor.Page1@38e00a5e

// scala> (new Page1[FaPage]).next.next.check2010.next.prev
// res5: kontur.scalatestrefactor.Page1[kontur.scalatestrefactor.FaPage] = kontur.scalatestrefactor.Page1@5e7175af

// (new Page1[FaPage]).next.next.check2010.next.next.next.next.prev.prev.prev.next.next
// res4: kontur.scalatestrefactor.CPage[kontur.scalatestrefactor.Page3[kontur.scalatestrefactor.FaPage],kontur.scalatestrefactor.FaPage] = kontur.scalatestrefactor.CPage@679e96f7


    print("Hello kontur.scalaTestRefactor!")
  }
}
