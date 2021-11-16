
import hb.dhbw.{FiniteClosure, RefType, TypeVariable, Unify, UnifyEqualsDot, UnifyLessDot, UnifyRefType, UnifyTV}
import org.scalatest.FunSuite

class UnifyTest extends FunSuite {
  val fcPair1 = (RefType("ArrayList", List(TypeVariable("A"))), RefType("List", List(TypeVariable("A"))))
  val fcPair3 = (RefType("List", List(TypeVariable("A"))), RefType("Object", List()))
  val fcPair2 = (RefType("MyMap", List(TypeVariable("A"), TypeVariable("B"))), RefType("Map", List(RefType("String", List(TypeVariable("A"))), TypeVariable("B"))))

  val fc = new FiniteClosure(Set())


  /*
  test("Unify.step2") {
    var step2 = Unify.step2(Set(UnifyLessDot(TypeVariable("a"), TypeVariable("b")),
      UnifyLessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List()))))), fc)
    println(step2)
    assert(!step2.isEmpty)

    step2 = Unify.step2(Set(UnifyLessDot(TypeVariable("a"), TypeVariable("b")),
      UnifyLessDot(RefType("List", List(RefType("Object", List()))), TypeVariable("a"))), fc)
    println(step2)
  }
  test("Unify.applyRules.WrongEQSet"){
    val unifyRes = Unify.unify(Set(Set(Set(UnifyEqualsDot(UnifyRefType("List", List()),UnifyRefType("Object", List()))))), fc)
    println(unifyRes)
    assert(!unifyRes.isEmpty)
  }
*/
}
