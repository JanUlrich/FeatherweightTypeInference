
import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("Parser.class"){
    println(hb.dhbw.Parser.parse(" class Test extends Object{}"))
  }

  test("Parser.methodCall"){
    println(fastparse.parse(("e.m(a)"), hb.dhbw.Parser.methodCall(_)))
  }

  test("Parser.constructor"){
    println(fastparse.parse(("new Test(a)"), hb.dhbw.Parser.constructor(_)))
  }

  test("Parser.constructorExpression"){
    println(fastparse.parse(("new Test(a)"), hb.dhbw.Parser.expr(_)))
  }

  test("Parser.type"){
    println(fastparse.parse("Test<Test<Object>>", hb.dhbw.Parser.typeParser(_)))
  }

  test("Parser.method"){
    println(fastparse.parse("m(a,b){return a;}", hb.dhbw.Parser.method(_)))
    println(fastparse.parse("m(){return a;}", hb.dhbw.Parser.method(_)))
    assert(fastparse.parse("m(){return a;}", hb.dhbw.Parser.method(_)).isSuccess)
    assert(fastparse.parse("m(a,b){return a.f;}", hb.dhbw.Parser.method(_)).isSuccess)
  }

  test("Parser.classDefinition"){
    println(fastparse.parse("class Test{\n m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)))
    assert(fastparse.parse("class Test{m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)).isSuccess)
  }
  test("Parser.classDefinition.generics"){
    println(fastparse.parse("class Test{\n m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)))
    assert(fastparse.parse("class Test{m(a,b){return a;}}", hb.dhbw.Parser.classDefinition(_)).isSuccess)
  }

  test("Parser.genericParamList"){
    println(fastparse.parse("<Test extends Object>", hb.dhbw.Parser.genericParamList(_)))
    println(fastparse.parse("<Test extends Object, Test<Object> extends Test<Object>>", hb.dhbw.Parser.genericParamList(_)))
  }
}
