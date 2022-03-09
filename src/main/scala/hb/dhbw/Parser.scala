package hb.dhbw

import fastparse._, fastparse.ScalaWhitespace._

final case class ParserClass(name: String, params: List[(NType,NType)], superType: NType, fields: List[(NType,String)], methods: List[ParserMethod])

final case class ParserMethod(retType: Option[NType], name: String, params: List[(Option[NType], String)], retExpr: ParserExpr)

sealed trait ParserExpr
final case class PLocalVar(x: String) extends ParserExpr
final case class PFieldVar(e: ParserExpr, f: String) extends ParserExpr
final case class PMethodCall(e: ParserExpr, name: String, params: List[ParserExpr]) extends ParserExpr
final case class PConstructor(className: String, params: List[ParserExpr]) extends ParserExpr
final case class PCast(to: NType, expr: ParserExpr) extends ParserExpr

final case class NType(name: String, params: List[NType])

object Parser {
  val keywords = Set("class", "new", "extends", "return")
  def kw[_: P](s: String) = s ~~ !(letter | digit)

  def letter[_: P]     = P( lowercase | uppercase )
  def lowercase[_: P]  = P( CharIn("a-z") )
  def uppercase[_: P]  = P( CharIn("A-Z") )
  def digit[_: P]      = P( CharIn("0-9") )
  def number[_: P]: P[Int] = P( CharIn("0-9").repX(1).!.map(_.toInt) )
  def ident[_: P]: P[String] =
    P( (letter) ~~ (letter | digit).repX ).!.filter(!keywords(_))

  def fieldVar[_: P]: P[ParserExpr] = P( ".".! ~ ident ).map(ite => PFieldVar(null, ite._2) )
  def prefixMethodCall[_: P]: P[ParserExpr] = P( "." ~ methodCall)
  def methodCall[_: P]: P[PMethodCall] = P( ident ~ paramList ).map(ite => PMethodCall(null, ite._1, ite._2) )
  def paramList[_: P] : P[List[ParserExpr]] =
    P("(".! ~ (expr ~ (",".! ~ expr).rep.map(_.toList.map{_._2})).? ~ ")".! )
    .map(ite => ite._2.map(params => params._1 :: params._2).getOrElse(List.empty))
  def variable[_: P]: P[ParserExpr] = P(ident).map(PLocalVar)
  def cast[_: P]: P[ParserExpr] = P("(" ~ typeParser ~ ")" ~ expr).map(x => PCast(x._1, x._2))
  def expr[_: P]: P[ParserExpr] = P( (variable | constructor | cast)~ (prefixMethodCall | fieldVar).rep.map(_.toList) )
    .map(ite => ite._2.foldLeft(ite._1) { (e1 : ParserExpr, e2 : ParserExpr) =>
      e2 match{
        case PMethodCall(_, name, e3) => PMethodCall(e1, name, e3)
        case PFieldVar(_, name) => PFieldVar(e1, name)
      }
    })

  def constructor[_: P]: P[ParserExpr] = P( kw("new") ~ methodCall).map(m => PConstructor(m.name,m.params))

  def classDefinition[_: P]: P[ParserClass] = P(kw("class") ~ ident ~ genericParamList.? ~ kw("extends") ~ typeParser ~ "{" ~ field.rep(0) ~ method.rep(0) ~ "}")
    .map(ite => ParserClass(ite._1, ite._2.getOrElse(List()),ite._3,  ite._4.toList, ite._5.toList))
  def field[_: P]: P[(NType, String)] = P(typeParser ~ ident ~ ";")
  def parameterDef[_ : P]: P[(Option[NType], String)] = P((typeParser.? ~ ident) | ident.map((None, _)))
  def method[_: P]: P[ParserMethod] =
    P(parameterDef ~ (("("~")").map(it => List()) | ("(" ~ parameterDef ~ ("," ~ parameterDef).rep(0) ~ ")")
      .map(ite => (ite._1, ite._2) +: ite._3.toList))
    ~ "{" ~ kw("return") ~ expr ~ ";" ~ "}")
    .map(ite => ParserMethod(ite._1, ite._2, ite._3, ite._4))
  def genericParamList[_: P]: P[List[(NType,NType)]] =
    P("<" ~ (typeParser ~ kw("extends") ~ typeParser) ~ ("," ~ (typeParser ~ kw("extends") ~ typeParser)).rep(0) ~ ">")
      .map(ite => List((ite._1, ite._2)) ++ ite._3.map(ite3 => (ite3._1, ite3._2)))
  def typeParser[_: P]: P[NType] = P(ident ~ ("<" ~ typeParser ~ ("," ~ typeParser).rep(0) ~  ">").?)
    .map(ite => NType(ite._1, ite._2.map(ite => List(ite._1) ++ ite._2).getOrElse(List())))

  def program[_: P]: P[List[ParserClass]] = "" ~ classDefinition.rep(1).map(_.toList) ~ End

  def parse(input: String): Either[String, List[ParserClass]] = fastparse.parse(input, program(_)).fold(
    (_, _, extra) => Left(s"Parser Error: ${extra.toString}"),
    (v, _) => Right(v)
  )
}
