import hb.dhbw.{ASTBuilder, FJTypeinference, InsertTypes, Main, Parser}
import org.scalatest.FunSuite

class IntegrationTest extends FunSuite {

  test("EmptyClass"){
    val ast = (fastparse.parse(("e.m(a)"), hb.dhbw.Parser.classDefinition(_)))
    //hb.dhbw.TYPE.TYPEClass(ast.get)
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
    println(result)
  }
  /*
  test("PaperExample"){
    val result = FJTypeinference.typeinference("class List<A extends Object> extends Object{\n  add( a){\n  return this;\n}\n}\nclass Test extends Object{\nm(a){ return a.add(this);}\n}")
    println(result.map(Main.prettyPrint(_)))
  }

  test("GenericVar"){
    val result = FJTypeinference.typeinference("class List<A extends Object> extends Object{\nA a;\n\nget(){ return this.a;\n\n}\n}\n\n\nclass Test extends Object{\nList<String> test;\n\nm(a){\n   return this.test.get();\n}\n\n}")
    println(result.map(Main.prettyPrint(_)))
  }

  test("IdentCallExample"){
    val result = FJTypeinference.typeinference("class Test extends Object{\n\n  m(a,b){return this.m(a);\n}\nm(a){return a;}\n}")
    println(result.map(Main.prettyPrint(_)))
  }

  test("IdentRecursive"){
    val result = FJTypeinference.typeinference("class Test extends Object{\n\nm(a){\nreturn this.m(a);\n}\n}")
    println(result.map(Main.prettyPrint(_)))
  }

  test("GetMethods"){
    val result = FJTypeinference.typeinference("class Test extends Object{\nget(){ return this.get().get();}\n}\n\nclass Test2 extends Object{\nget(){ return this;}\n}" )
    println(result.map(Main.prettyPrint(_)))
  }

  test("constructorTest"){
    val input= "class Test extends Object{m(){ return new Test();}}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(Main.prettyPrint(_)))
  }

  test("fieldVar access"){
    val input ="class List<A extends Object> extends Object{\nA f;\nget(){ return this.f; }\n}\n\nclass Test2 extends Object{\nget(){ return new List(this).get();}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(Main.prettyPrint(_)))
  }

  test("constructor.FieldInitialization") {
    val input = "class List<A extends Object> extends Object{\nA f;\n  add(a){\n  return new List(a);\n}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(Main.prettyPrint(_)))
  }


   */
  test("list.add") {
    val input = "class List<A extends Object> extends Object{\nA f;\n  add( a){\n  return new List(a);\n}\n}\nclass Test extends Object{\n\nm(a){return a.add(this);}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrint(it._1)))

  }
}
