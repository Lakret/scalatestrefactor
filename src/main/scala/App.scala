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

trait ContainsState[State] {

}

trait ExpertPage extends FillMinimal


//TODO: skipper
//TODO: different final pages

object Ops {
  def skip[A <: ExpertPage, B <: ExpertPage](a : A)(implicit ev : A => WizardNextStep[A, B]) : B =
    a.fillMinimal.next
}

//pages predefined
class Page1 extends ExpertPage

object Page1 {
  //pages to construct
  //we can use reflection or macros to autogenerate it
  implicit val page1ToConstruct: Construct[Page1] =
    new Page1 with Construct[Page1] {
      def get = new Page1
    }

  //pages to steps
  //we can use macros to autogenerate it
  implicit def page1ToNextStep(x : Page1): WizardNextStep[Page1, Page2] =
    new Page1 with WizardNextStep[Page1, Page2] {
      def next = implicitly[Construct[Page2]].get
    }
}

class Page2 extends ExpertPage

object Page2 {
  implicit val page2ToConstruct: Construct[Page2] =
    new Page2 with Construct[Page2] {
      def get = new Page2
    }


  implicit def page2ToNextStep(x : Page2): WizardNextStep[Page2, CPage[Page3]] =
    new Page2 with WizardNextStep[Page2, CPage[Page3]] {
      def next = implicitly[Construct[CPage[Page3]]].get
    }

  implicit def page2ToPrevStep(x : Page2): WizardPrevStep[Page2, Page1] =
    new Page2 with WizardPrevStep[Page2, Page1] {
      def prev = implicitly[Construct[Page1]].get
    }
}

class Page3 extends ExpertPage

object Page3 {
  implicit val page3ToConstruct: Construct[Page3] =
    new Page3 with Construct[Page3] {
      def get = new Page3
    }

  implicit def page3ToPrevStep(x : Page3): WizardPrevStep[Page3, Page2] =
    new Page3 with WizardPrevStep[Page3, Page2] {
      def prev = implicitly[Construct[Page2]].get
    }
}

//this page can have several next pages
class CPage[Next <: ExpertPage] extends ExpertPage {
  def check2010 = new CPage[Page2]
}

object CPage {
  def apply() = new CPage[Page3]

  implicit def cPageToConstruct[A <: ExpertPage]: Construct[CPage[A]] =
    new CPage with Construct[CPage[A]] {
      def get = new CPage[A]
    }

  implicit def CPageToNextStep[Next <: ExpertPage : Construct](x : CPage[Next]) : WizardNextStep[CPage[Next], Next] =
    new CPage[Next] with WizardNextStep[CPage[Next], Next] {
      def next = implicitly[Construct[Next]].get
    }

  implicit def CPageToPrevStep[Next <: ExpertPage : Construct](x : CPage[Next]) : WizardPrevStep[CPage[Next], Page3] =
    new CPage[Next] with WizardPrevStep[CPage[Next], Page3] {
      def prev = new Page3
    }
}

//generic traits
trait Construct[A] {
  def get : A
}

trait WizardNextStep[Current <: ExpertPage, Next <: ExpertPage] {
  def next : Next
}

trait WizardPrevStep[Current <: ExpertPage, Prev <: ExpertPage] {
  def prev : Prev
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
    // scala> CPage().proceed
    // res1: kontur.scalatestrefactor.Page3 = kontur.scalatestrefactor.Page3@187293dd

    // scala> implicitly[Construct[CPage[Page3]]].get
    // res3: kontur.scalatestrefactor.CPage[kontur.scalatestrefactor.Page3] = kontur.sc
    // alatestrefactor.CPage@515441f

    // scala> res0.proceed.proceed.check2010.proceed.prev
    // res5: kontur.scalatestrefactor.Page1 = kontur.scalatestrefactor.Page1@71471ecf


    print("Hello kontur.scalaTestRefactor!")
  }
}
