package hb.dhbw

import hb.dhbw.Unify.{Constraint, EqualsDot, LessDot}
;

object Main {

  def main(args: Array[String]): Unit = {
    val fcPair1 = (RefType("ArrayList", List(TypeVariable("A"))), RefType("List", List(TypeVariable("A"))))
    val fcPair3 = (RefType("List", List(TypeVariable("A"))), RefType("Object", List()))
    val fcPair2 = (RefType("MyMap", List(TypeVariable("A"), TypeVariable("B"))), RefType("Map", List(RefType("String", List(TypeVariable("A"))), TypeVariable("B"))))
    val cTest = Set(Set(1,2), Set(4,3))
    val fc = new FiniteClosure(Set(fcPair1, fcPair2, fcPair3))
    val cart = Unify.cartesianProduct(cTest)
    println(cart)

    val superTypes = fc.superTypes(RefType("List", List(RefType("Object", List()))))
    println(superTypes)

    val step1 = Unify.step1(Set(LessDot(TypeVariable("a"), TypeVariable("b")),
      LessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List()))))), fc)
    println(step1)

    println(fc.isPossibleSupertype("MyMap", "List"))

    val eqMatchTest = Set(LessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List())))), LessDot(TypeVariable("a"), RefType("ArrayList", List(RefType("Object", List())))))
    println(Unify.matchRule(eqMatchTest.asInstanceOf[Set[Unify.Constraint]], fc))

    val eqEqualsTest : Set[Constraint] = Set(LessDot(TypeVariable("a"), TypeVariable("b")),LessDot(TypeVariable("b"), TypeVariable("c")), LessDot(TypeVariable("c"), TypeVariable("a")))
    println(Unify.equalsRule(eqEqualsTest))

    //val eqIsLinkedTest = Set(LessDot(TypeVariable("a"), TypeVariable("c")), LessDot(TypeVariable("b"), TypeVariable("c")))
    //println(Unify.isLinked(TypeVariable("a"), TypeVariable("b"), eqIsLinkedTest))

    val eqAdoptTest = Set(LessDot(TypeVariable("b"), TypeVariable("a")),LessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List())))), LessDot(TypeVariable("b"), RefType("ArrayList", List(RefType("Object", List())))))
    println(Unify.adoptRule(eqAdoptTest.asInstanceOf[Set[Constraint]], fc))

    val eqAdaptTest: Set[Constraint] = Set(EqualsDot(TypeVariable("a"), TypeVariable("b")),LessDot(RefType("ArrayList",List(RefType("Test",List()))),RefType("List",List(RefType("Object",List())))))
    println(Unify.adaptRule(eqAdaptTest, fc))

    val eqReduceTest : Set[Constraint] = Set(EqualsDot(RefType("List",List(RefType("Test",List()))),RefType("List",List(RefType("Object",List())))))
    println(Unify.reduceRule(eqReduceTest))
    //val replacedType = fc.replaceParams(RefType("List", List(TypePlaceholder("A"))), RefType("ArrayList", List(RefType("String", List()))))
    //println(replacedType)
    //val replacedType2 = fc.replaceParams(RefType("Map", List(RefType("String", List(TypePlaceholder("A"))), TypePlaceholder("B"))), RefType("MyMap", List(RefType("String", List()), RefType("Integer", List()))))
    //println(replacedType2)
  }
}