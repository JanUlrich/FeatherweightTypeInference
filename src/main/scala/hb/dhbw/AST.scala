package hb.dhbw

final case class Class(name: String, genericParams: List[(Type,Type)], superType: RefType, fields: List[(Type,String)], methods: List[Method])
final case class Method(genericParams: List[Constraint], retType: Type, name: String, params: List[(Type, String)], retExpr: Expr)

sealed trait Type
final case class RefType(name: String, params: List[Type]) extends Type
final case class GenericType(name: String) extends Type
final case class TypeVariable(name: String) extends Type

sealed trait Expr
final case class LocalVar(x: String) extends Expr
final case class FieldVar(e: Expr, f: String) extends Expr
final case class MethodCall(e: Expr, name: String, params: List[Expr]) extends Expr
final case class Constructor(className: String, params: List[Expr]) extends Expr
final case class Cast(to: Type, expr: Expr) extends Expr

object ASTBuilder {
  def fromParseTree(toAst: List[ParserClass]) = new ASTBuilderMonad().fromParseTree(toAst)

  private class ASTBuilderMonad{

    var tpvNum = 0

    def fromParseTree(toAst: List[ParserClass]) = toAst.map(c => {
      val genericNames = c.params.map(_._1).map(_.name).toSet
      Class(c.name, c.params.map(p => (nTypeToType(p._1, genericNames), nTypeToType(p._2, genericNames))),
        nTypeToType(c.superType, genericNames).asInstanceOf[RefType],
        c.fields.map(f => (nTypeToType(f._1, genericNames),f._2)), c.methods.map(m => Method(List(), freshTPV(), m.name,
          m.params.map(p => (p._1.map(it => nTypeToType(it, genericNames)).getOrElse(freshTPV()), p._2)),
          fromParseExpr(m.retExpr, genericNames))))
    })

    def fromParseExpr(from: ParserExpr, genericNames: Set[String]): Expr = from match{
      case PMethodCall(e, name, params) => MethodCall(fromParseExpr(e, genericNames), name, params.map(fromParseExpr(_, genericNames)))
      case PConstructor(className, params) => Constructor(className, params.map(fromParseExpr(_, genericNames)))
      case PFieldVar(e, f) => FieldVar(fromParseExpr(e, genericNames), f)
      case PCast(ntype, e) => Cast(nTypeToType(ntype, genericNames), fromParseExpr(e, genericNames))
      case PLocalVar(n) => LocalVar(n)
    }

    private def freshTPV() = {
      def numToLetter(num: Int) = {
        val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var ret = ""
        var n = num
        while (n > alphabet.length){
          val char = num % alphabet.length
          n= num / alphabet.length
          ret = ret + alphabet.charAt(char)
        }
        ret + alphabet.charAt(n)
      }
      tpvNum = tpvNum+1
      TypeVariable(numToLetter(tpvNum))
    }

    private def nTypeToType(t : NType, genericNames: Set[String]): Type = if(t.params.isEmpty && genericNames.contains(t.name)) {
      GenericType(t.name)
    }else{
      RefType(t.name, t.params.map(p => nTypeToType(p, genericNames)))
    }
  }
}
