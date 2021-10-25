package hb.dhbw

import fastparse._, fastparse.ScalaWhitespace._

final case class ParserClass(name: String, params: List[(NType,NType)], superType: NType, fields: List[(NType,String)], methods: List[ParserMethod])

final case class ParserMethod(name: String, params: List[String], retExpr: Expr)
final case class NType(name: String, params: List[NType])

object Parser {
  val keywords = Set("class", "new", "extends")
  def kw[_: P](s: String) = s ~~ !(letter | digit | "_" | "'")

  def letter[_: P]     = P( lowercase | uppercase )
  def lowercase[_: P]  = P( CharIn("a-z") )
  def uppercase[_: P]  = P( CharIn("A-Z") )
  def digit[_: P]      = P( CharIn("0-9") )
  def number[_: P]: P[Int] = P( CharIn("0-9").repX(1).!.map(_.toInt) )
  def ident[_: P]: P[String] =
    P( (letter | "_") ~~ (letter | digit | "_" | "'").repX ).!.filter(!keywords(_))

  def fieldVar[_: P]: P[Expr] = P( ".".! ~ ident ).map(ite => FieldVar(null, ite._2) )
  def prefixMethodCall[_: P]: P[Expr] = P( "." ~ methodCall)
  def methodCall[_: P]: P[MethodCall] =P( ident ~ "(".! ~ (expr ~ (",".! ~ expr).rep.map(_.toList.map{_._2})).? ~ ")".! ).map(ite => MethodCall(null, ite._1, ite._3.map { ite => ite._1 :: ite._2 } .getOrElse(List.empty)) )
  def variable[_: P]: P[Expr] = P(ident).map(LocalVar)
  def expr[_: P]: P[Expr] = P( variable ~ (prefixMethodCall | fieldVar).rep.map(_.toList) )
    .map(ite => ite._2.foldLeft(ite._1) { (e1 : Expr, e2 : Expr) =>
      e2 match{
        case MethodCall(_, name, e3) => MethodCall(e1, name, e3)
        case FieldVar(_, name) => FieldVar(e1, name)
      }
    })

  def constructor[_: P]: P[Expr] = P( kw("new") ~ methodCall).map(m => Constructor(m.name,m.params))

  def classDefinition[_: P]: P[ParserClass] = P(kw("class") ~ ident ~ genericParamList.? ~ kw("extends") ~ typeParser ~ "{" ~ field.rep(0) ~ method.rep(0) ~ "}")
    .map(ite => ParserClass(ite._1, ite._2.getOrElse(List()),ite._3,  ite._4.toList, ite._5.toList))
  def field[_: P]: P[(NType, String)] = P(typeParser ~ ident ~ ";")
  def method[_: P]: P[ParserMethod] = P(ident ~ (("("~")").map(it => List()) | ("(" ~ ident ~ ("," ~ ident).rep(0) ~ ")").map(ite => List(ite._1) ++ ite._2))
    ~ "{" ~ kw("return") ~ expr ~ ";" ~ "}")
    .map(ite => ParserMethod(ite._1, ite._2, ite._3))
  def genericParamList[_: P]: P[List[(NType,NType)]] = P("<" ~ (typeParser ~ kw("extends") ~ typeParser) ~ ("," ~ (typeParser ~ kw("extends") ~ typeParser)).rep(0) ~ ">").map(ite => List((ite._1, ite._2)) ++ ite._3.map(ite3 => (ite3._1, ite3._2)))
  def typeParser[_: P]: P[NType] = P(ident ~ ("<" ~ typeParser ~ ("," ~ typeParser).rep(0) ~  ">").?)
    .map(ite => NType(ite._1, ite._2.map(ite => List(ite._1) ++ ite._2).getOrElse(List())))

  def program[_: P]: P[List[ParserClass]] = "" ~ classDefinition.rep(1).map(_.toList) ~ End

  def parse(input: String): Either[String, List[ParserClass]] = fastparse.parse(input, program(_)).fold(
    (_, _, extra) => Left(s"Parser Error: $extra"),
    (v, _) => Right(v)
  )
}
