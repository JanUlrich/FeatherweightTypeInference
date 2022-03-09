
import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("Parser.class"){
    println(hb.dhbw.Parser.parse(" class Test extends Object{}"))
  }

  test("Parser.methodCall"){
    println(fastparse.parse(("e.m(a)"), hb.dhbw.Parser.expr(_)))
  }

  test("Parser.constructor"){
    println(fastparse.parse(("new Test(a)"), hb.dhbw.Parser.constructor(_)))
    println(fastparse.parse("new List(this)", hb.dhbw.Parser.expr(_)))
    println(fastparse.parse("m(){return new List(this);}", hb.dhbw.Parser.method(_)))
  }

  test("Parser.constructorExpression"){
    println(fastparse.parse(("new Test(a)"), hb.dhbw.Parser.expr(_)))
  }

  test("Parser.type"){
    println(fastparse.parse("Test<Test<Object>>", hb.dhbw.Parser.typeParser(_)))
  }
  test("Parser.cast"){
    println(fastparse.parse("(Test<Test<Object>>) m", hb.dhbw.Parser.expr(_)))
    println(fastparse.parse("(Test<Test<Object>>) this.m()", hb.dhbw.Parser.expr(_)))
    println(fastparse.parse("(X)(Test<Test<X>>) m.f", hb.dhbw.Parser.expr(_)))
    println(fastparse.parse("(Test<Object>)(X) this", hb.dhbw.Parser.expr(_)))
    println(fastparse.parse("class Test extends Object{ m(){return (Test<Object>)(X) this;}}", hb.dhbw.Parser.program(_)))
  }

  test("Parser.method"){
    println(fastparse.parse("m(a,b){return a;}", hb.dhbw.Parser.method(_)))
    println(fastparse.parse("m(){return a;}", hb.dhbw.Parser.method(_)))
    assert(fastparse.parse("m(){return a;}", hb.dhbw.Parser.method(_)).isSuccess)
    assert(fastparse.parse("m(a,b){return a.f;}", hb.dhbw.Parser.method(_)).isSuccess)
  }

  test("Parser.classDefinition"){
    println(fastparse.parse("class Test extends Object{\n m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)))
    assert(fastparse.parse("class Test extends Object {m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)).isSuccess)
  }
  test("Parser.classDefinition.generics"){
    println(fastparse.parse("class Test extends Object {\n m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)))
    assert(fastparse.parse("class Test extends Object {m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)).isSuccess)
  }

  test("Parser.genericParamList"){
    println(fastparse.parse("<Test extends Object>", hb.dhbw.Parser.genericParamList(_)))
    println(fastparse.parse("<Test extends Object, Test<Object> extends Test<Object>>", hb.dhbw.Parser.genericParamList(_)))
  }

  test("Parser.this"){
    println(fastparse.parse("this", hb.dhbw.Parser.expr(_)))
    println(fastparse.parse("this.m()", hb.dhbw.Parser.expr(_)))
  }

  test("Parser.test"){
    println(fastparse.parse("class List<A extends Object> extends Object{asd(){ return this;  }get(){ return this.head;}}"
      , hb.dhbw.Parser.program(_)))
  }
}
