package hb.dhbw

import hb.dhbw.Unify.LessDot
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

    val step1 = Unify.step1(Set(LessDot(TypeVariable("a"), TypeVariable("b")),
      LessDot(TypeVariable("a"), RefType("List", List(RefType("Object", List()))))), fc)
    println(step1)
    //val replacedType = fc.replaceParams(RefType("List", List(TypePlaceholder("A"))), RefType("ArrayList", List(RefType("String", List()))))
    //println(replacedType)
    //val replacedType2 = fc.replaceParams(RefType("Map", List(RefType("String", List(TypePlaceholder("A"))), TypePlaceholder("B"))), RefType("MyMap", List(RefType("String", List()), RefType("Integer", List()))))
    //println(replacedType2)
  }
}