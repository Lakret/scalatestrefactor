
trait WizardStep[Prev, Current, Next] extends WizardStepOps[Prev, Current, Next] {
  def next : Next
  def prev : Prev
}

//we can extend WizardStep
trait WizardStepOps[Prev, Current, Next] {
  self : WizardStep[Prev, Current, Next] =>

  def proceed2 : Next =
    next
}

//we can define functions for viewable as WizardStep
def proceed[A <% WizardStep[C, A, B], B, C](x : A) : B =
    (x : WizardStep[C, A, B]).next


class Page1
class Page2
class Page3

implicit def page2ToStep(x : Page2): WizardStep[Page1, Page2, Page3] =
  new Page2 with WizardStep[Page1, Page2, Page3] {
    def next = new Page3
    def prev = new Page1
  }

proceed(new Page2)

//now, what will we do, if page can point in two directions?

//first, we must know, how to construct any page
//(later on will be substitued with reflection)
trait Construct[A] {
  def get : A
}

implicit val page1ToConstruct: Construct[Page1] =
  new Page1 with Construct[Page1] {
    def get = new Page1
  }

implicit val page2ToConstruct: Construct[Page2] =
  new Page2 with Construct[Page2] {
    def get = new Page2
  }


//if 2010 - go to Page2, otherwise - Page1
//higher kinds

class CPage[Next] {
  def check2010 = new CPage[Page2]
}

object CPage {
  def apply() = new CPage[Page1]
}

//now wee can reuse WizardStep! for both cases!!1
implicit def CPageToStep[Next : Construct](x : CPage[Next]) : WizardStep[Page3, CPage[Next], Next] =
  new CPage[Next] with WizardStep[Page3, CPage[Next], Next] {
    def next = implicitly[Construct[Next]].get
    def prev = new Page3
  }

// scala> CPage().proceed2
// res46: Page1 = Page1@b0ae00b

// scala> CPage().check2010.proceed2
// res48: Page2 = Page2@667209a3

//but we still need to reassign pages. I want to track a current page implicitly
//State monad to the rescue!

//what is our state? our state is a current page!

import scalaz._
import Scalaz._
