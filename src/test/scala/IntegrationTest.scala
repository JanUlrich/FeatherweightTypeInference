import hb.dhbw.{FJTypeinference, Main}
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
  test("PaperExample"){
    val result = FJTypeinference.typeinference("class List<A extends Object> extends Object{\n  add( a){\n  return this;\n}\n}\nclass Test extends Object{\nm(a){ return a.add(this);}\n}")
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

}
