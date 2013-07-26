package kontur.scalatestrefactor

import scalaz._
import Scalaz._

trait FillMinimal {
  def fillMinimal {}
}

trait ExpertPage

//TODO: skipper
//TODO: different final pages

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
  implicit def page1ToStep(x : Page1): WizardStep[Page1, Page1, Page2] =
    new Page1 with WizardStep[Page1, Page1, Page2] {
      def next = implicitly[Construct[Page2]].get
      def prev = this
    }
}

class Page2 extends ExpertPage

object Page2 {
  implicit val page2ToConstruct: Construct[Page2] =
    new Page2 with Construct[Page2] {
      def get = new Page2
    }
  
  
  implicit def page2ToStep(x : Page2): WizardStep[Page1, Page2, CPage[Page3]] =
    new Page2 with WizardStep[Page1, Page2, CPage[Page3]] {
      def next = implicitly[Construct[CPage[Page3]]].get
      def prev = implicitly[Construct[Page1]].get
    }
}

class Page3 extends ExpertPage

object Page3 {
  implicit val page3ToConstruct: Construct[Page3] =
    new Page3 with Construct[Page3] {
      def get = new Page3
    }
  
  implicit def page3ToStep(x : Page3): WizardStep[Page2, Page3, Page3] =
    new Page3 with WizardStep[Page2, Page3, Page3] {
      def next = this
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
  
  implicit def CPageToStep[Next <: ExpertPage : Construct](x : CPage[Next]) : WizardStep[Page3, CPage[Next], Next] =
    new CPage[Next] with WizardStep[Page3, CPage[Next], Next] {
      def next = implicitly[Construct[Next]].get
      def prev = new Page3
    }
}

//generic traits
trait Construct[A] {
  def get : A
}

trait WizardStep[Prev <: ExpertPage, Current <: ExpertPage, Next<: ExpertPage]
      extends WizardStepOps[Prev, Current, Next] {
  def next : Next
  def prev : Prev
}

//we can extend WizardStep
trait WizardStepOps[Prev <: ExpertPage, Current <: ExpertPage, Next <: ExpertPage] {
  self : WizardStep[Prev, Current, Next] =>

  def proceed : Next = next
}

object MonadicTester {  
  //try to devise underlying type with following props:
  // - knows who is current, next and prev (if available)
  // - makes all of them implicitly available
  // - knows if page is present

  def startWith = state[Option[Page1], Page1](new Page1)
  def goFurther[Prev <: ExpertPage, Current <: ExpertPage <% WizardStep[Prev, Current, Next], Next <: ExpertPage] = 
    State[Option[Current], Next] { case old@Some(x) =>
      val next = x.proceed
      (old, next)
    }
  
  for {
    x <- startWith
    _ <- goFurther[Page1, Page1, Page2] //how to drop explicit arguments?
  } yield(x)
}

object MonadicTester2 {  
  trait Underlying {
    type Current <: ExpertPage
  }
  case class InWizard[Prev <: ExpertPage, Next <: ExpertPage](curr : Underlying#Current)
  					(implicit ev : Underlying#Current => 
  					   WizardStep[Prev, Underlying#Current, Next]) 
  	   extends Underlying {
    type P = Prev
    type N = Next
    def asWizard = ev(curr)
  }
  case class NotInWizard() extends Underlying {
    type Page <: Option[ExpertPage]
  }

  def startWith = state[Underlying, Page1](new Page1)
  def goFurther[A <: ExpertPage, B <: ExpertPage] = 
    State[Underlying, InWizard[A, B]#N] { case old@InWizard(x) =>
      val next = old.asWizard.proceed
      (old, next.asInstanceOf[B])
    }
  
  val fin = 
    for {
      x <- startWith
      _ <- goFurther[Page1, Page2] //how to drop explicit arguments?
      fin <- goFurther[Page2, Page3]
	} yield(fin)
}



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
