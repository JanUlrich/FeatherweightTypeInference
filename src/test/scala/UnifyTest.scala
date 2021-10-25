
import hb.dhbw.{FiniteClosure, RefType, TypeVariable, Unify, UnifyEqualsDot, UnifyLessDot}
import org.scalatest.FunSuite

class UnifyTest extends FunSuite {
  val fcPair1 = (RefType("ArrayList", List(TypeVariable("A"))), RefType("List", List(TypeVariable("A"))))
  val fcPair3 = (RefType("List", List(TypeVariable("A"))), RefType("Object", List()))
  val fcPair2 = (RefType("MyMap", List(TypeVariable("A"), TypeVariable("B"))), RefType("Map", List(RefType("String", List(TypeVariable("A"))), TypeVariable("B"))))

  val fc = new FiniteClosure(Set(fcPair1, fcPair2, fcPair3))

  test("Unify.step2.alinkedb"){
    var step2 = Unify.step2(Set(UnifyLessDot(TypeVariable("c"), TypeVariable("b")),
      UnifyLessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List()))))), fc)
    assert(step2.head.size == 2)
  }

  test("Unify.step2") {
    var step2 = Unify.step2(Set(UnifyLessDot(TypeVariable("a"), TypeVariable("b")),
      UnifyLessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List()))))), fc)
    println(step2)
    assert(!step2.isEmpty)

    step2 = Unify.step2(Set(UnifyLessDot(TypeVariable("a"), TypeVariable("b")),
      UnifyLessDot(RefType("List", List(RefType("Object", List()))), TypeVariable("a"))), fc)
    println(step2)
  }

}
