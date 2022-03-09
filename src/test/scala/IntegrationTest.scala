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
   */

  test("constructor.FieldInitialization") {
    val input = "class List<A extends Object> extends Object{\nA f;\n  add(a){\n  return new List(a);\n}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }
  test("list.add") {
    val input = "class List<A extends Object> extends Object{\nA f;\n  add( a){\n  return new List(a);\n}\n}\nclass Test extends Object{\n\nm(a){return a.add(this);}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }
  test("list.add.2") {
    val input = "class List<A extends Object> extends Object{\n  A head;\n  List<A> tail;\n  add( a){\n  return new List(a, this);\n}\nget(){\nreturn this.head;\n}\n}\n\nclass Test extends Object{\nm(a){\nreturn a.add(this).get();\n}\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }

  test("functionClass") {
    val input = "class SameType<A extends Object, B extends Object> extends Object{\nA a;\nA b;\nB c;\nget(){return this.c;}\n}\nclass Function<A extends Object, B extends Object> extends Object{\nA ret;\nB param;\napply(a){\nreturn new SameType(this.param, a, this).get().ret;\n}\n\n}"
    val result = FJTypeinference.typeinference(input )
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }

  test("TwoRecursiveMethods") {
    val input = "class RecursiveMethods extends Object{\n\na1(x){ return this.a2(x);}\na2(y){ return this.a1(y);}}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }

  test("Function.typeAnnotaded") {
    val input = "\nclass Function<A extends Object, B extends Object> extends Object{\nB b;\nB apply(A a){\nreturn this.b;\n}\n\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }

  test("Box.Map") {
    val input = "class Function<A extends Object, B extends Object> extends Object{\nB b;\nB apply(A a){\nreturn this.b;\n}\n}\n\n\nclass Box<S extends Object> extends Object {\nS val ;\nmap( f ) {\nreturn new Box(f.apply(this.val)) ;\n}\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }

  test("PrincipalType") {
    val input = "\nclass List<A extends Object> extends Object{\n  A head;\n  List<A> tail;\n  add( a){\n    return new List(a, this);\n  }\n  get(){\n    return this.head;\n  }\n}\n\nclass PrincipleType extends Object {\n  function(a){\n    return a.add(this).get();\n  }\n}"
    val result = FJTypeinference.typeinference(input)
    println(result.map(it => Main.prettyPrintAST(it._2)))
  }
  /*

class List<A extends Object> extends Object{
  A head;
  List<A> tail;
  add( a){
    return new List(a, this);
  }
  get(){
    return this.head;
  }
}

class PrincipleType extends Object {
  function(a){
    return a.add(this).get();
  }
}


class Function<A extends Object, B extends Object> extends Object{
B b;
B apply(A a){
return this.b;
}
}


class Box<S extends Object> extends Object {
S val ;
map( f ) {
return new Box(f.apply(this.val)) ;
}
}

class Function<A extends Object, B extends Object> extends Object{
B b;
B apply(A a){
return this.b;
}

}


class RecursiveMethods extends Object{

a1(x){ return this.a2(x);}
a2(y){ return this.a1(y);}
}
   */
    /*
  Additional Tests:

class Test extends Object{
 m(a, b){return a;}
m(a,b){return b;}
}

class Test2 extends Object{
test(a){return new Test().m(this,a);}
}

   */
}
