

import hb.dhbw.{FJNamedType, FiniteClosure, RefType, TypeVariable, Unify, UnifyConstraint, UnifyEqualsDot, UnifyLessDot, UnifyRefType, UnifyTV}
import org.scalatest.FunSuite

class UnifyTest extends FunSuite {
  val fcPair1 = (RefType("ArrayList", List(TypeVariable("A"))), RefType("List", List(TypeVariable("A"))))
  val fcPair3 = (RefType("List", List(TypeVariable("A"))), RefType("Object", List()))
  val fcPair2 = (RefType("MyMap", List(TypeVariable("A"), TypeVariable("B"))), RefType("Map", List(RefType("String", List(TypeVariable("A"))), TypeVariable("B"))))

  val fc = new FiniteClosure(Set())

  test("sub-elim rule"){
    val input : Set[UnifyConstraint] = Set(UnifyLessDot(UnifyTV("a"), UnifyTV("b")), UnifyEqualsDot(UnifyTV("a"), UnifyRefType("a", List())))
    val result = Unify.postProcessing(input)
    println(result)
    assert(result.contains(UnifyEqualsDot(UnifyTV("a"), UnifyTV("b"))))
    assert(result.contains(UnifyEqualsDot(UnifyTV("b"), UnifyTV("a"))))
  }

  test("error"){
    val input : Set[Set[Set[UnifyConstraint]]]= Set(Set(Set(UnifyLessDot(UnifyTV("1"), UnifyTV("B")), UnifyLessDot(UnifyTV("1"), UnifyTV("2")),
      UnifyLessDot(UnifyRefType("Test", List()), UnifyTV("2")))))
    val result = Unify.unifyIterative(input, new FiniteClosure(Set((FJNamedType("Test", List()), FJNamedType("Object", List())))))
    println(result)
  }

  test("example"){
    val input : Set[UnifyConstraint] = Set(UnifyEqualsDot(UnifyTV("b"), UnifyTV("y")),
      UnifyLessDot(UnifyRefType("Pair", List(UnifyRefType("X", List()), UnifyRefType("Y", List()))), UnifyRefType("Pair", List(UnifyTV("w"), UnifyTV("y")))),
      UnifyLessDot(UnifyRefType("Pair", List(UnifyTV("d"), UnifyTV("e"))), UnifyTV("A")),
      UnifyLessDot(UnifyTV("F"), UnifyTV("d")),
      UnifyLessDot(UnifyTV("b"), UnifyTV("e")),
      UnifyLessDot(UnifyTV("F"), UnifyRefType("Object", List())))
    val result = Unify.unifyIterative(Set(Set(input)), new FiniteClosure(Set((FJNamedType("Pair", List(FJNamedType("X", List()),FJNamedType("X", List()))), FJNamedType("Object", List())))))
    println(result)
    //assert(result.contains(UnifyEqualsDot(UnifyTV("a"), UnifyTV("b"))))
    //assert(result.contains(UnifyEqualsDot(UnifyTV("b"), UnifyTV("a"))))
  }
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
