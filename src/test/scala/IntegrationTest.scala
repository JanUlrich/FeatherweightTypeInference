import hb.dhbw.{ASTBuilder, FJTypeinference, InsertTypes, Main, Parser}
import org.scalatest.FunSuite

class IntegrationTest extends FunSuite {

  test("EmptyClass"){
    val ast = (fastparse.parse(("e.m(a)"), hb.dhbw.Parser.classDefinition(_)))
    //hb.dhbw.TYPE.TYPEClass(ast.get)
    val result = FJTypeinference.typeinference("class Test extends Object {\n\n}")
    println(result)
  }

  test("IdMethod"){
    val result = FJTypeinference.typeinference("class Test extends Object {\nObject f;\nm(a){return a;}\n}")
    println(result)
  }

  test("IdMethodRecursive"){
    val result = FJTypeinference.typeinference("class Test extends Object {\n   Object f;\n   m(a, b){return this.m(b, a); }\n}")
    println(result)
  }

  test("ListAddDefinition"){
    val result = FJTypeinference.typeinference("class List<A extends Object> extends Object{\n  add(a){\n  return this;\n}\n}")
    println(result.map(Main.prettyPrintAST(_)))
  }

  test("constructor.FieldInitialization") {
    val input = "class List<A extends Object> extends Object{\nA f;\n  add(a){\n  return new List(a);\n}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it)))
  }
  test("list.add") {
    val input = "class List<A extends Object> extends Object{\nA f;\n  add( a){\n  return new List(a);\n}\n}\nclass Test extends Object{\n\nm(a){return a.add(this);}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it)))
  }
  test("list.add.2") {
    val input = "class List<A extends Object> extends Object{\n  A head;\n  List<A> tail;\n  add( a){\n  return new List(a, this);\n}\nget(){\nreturn this.head;\n}\n}\n\nclass Test extends Object{\nm(a){\nreturn a.add(this).get();\n}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it)))
  }

  test("functionClass") {
    val input = "class SameType<A extends Object, B extends Object> extends Object{\nA a;\nA b;\nB c;\nget(){return this.c;}\n}\nclass Function<A extends Object, B extends Object> extends Object{\nA ret;\nB param;\napply(a){\nreturn new SameType(this.param, a, this).get().ret;\n}\n\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it)))
  }

  test("TwoRecursiveMethods") {
    val input = "class RecursiveMethods extends Object{\n\na1(x){ return this.a2(x);}\na2(y){ return this.a1(y);}}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it)))
  }

  test("Function.typeAnnotaded") {
    val input = "\nclass Function<A extends Object, B extends Object> extends Object{\nB b;\nB apply(A a){\nreturn this.b;\n}\n\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it)))
  }

  test("Box.Map") {
    val input = "class Function<A extends Object, B extends Object> extends Object{\nB b;\nB apply(A a){\nreturn this.b;\n}\n}\n\n\nclass Box<S extends Object> extends Object {\nS val ;\nmap( f ) {\nreturn new Box(f.apply(this.val)) ;\n}\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it)))
  }

  test("PrincipalType") {
    val input = "\nclass List<A extends Object> extends Object{\n  A head;\n  List<A> tail;\n  add( a){\n    return new List(a, this);\n  }\n  get(){\n    return this.head;\n  }\n}\n\nclass PrincipleType extends Object {\n  function(a){\n    return a.add(this).get();\n  }\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it)))
  }

  test("ListExample") {
    val input = "\n\nclass List<A extends Object> extends Object{\n    A element;\n    List<A> succ; \n    add(a){\n       return new List(a, this);\n    }\n\n}\n\nclass Example extends Object{\n\n    test(a){\n        return a.add(this);\n    }\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it)))
  }
}
