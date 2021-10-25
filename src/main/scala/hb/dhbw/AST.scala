package hb.dhbw

final case class Class(name: String, params: List[(Type,Type)], superType: RefType, fields: List[(Type,String)], methods: List[Method])
final case class Method(retType: Type, name: String, params: List[(Type, String)], retExpr: Expr)

sealed trait Type
final case class RefType(name: String, params: List[Type]) extends Type
final case class TypeVariable(name: String) extends Type

sealed trait Expr
final case class LocalVar(x: String) extends Expr
final case class FieldVar(e: Expr, f: String) extends Expr
final case class MethodCall(e: Expr, name: String, params: List[Expr]) extends Expr
final case class Constructor(className: String, params: List[Expr]) extends Expr

object ASTBuilder {
  def fromParseTree(toAst: List[ParserClass]) = new ASTBuilderMonad().fromParseTree(toAst)

  private class ASTBuilderMonad{

    var tpvNum = 0

    def fromParseTree(toAst: List[ParserClass]) = toAst.map(c => Class(c.name, c.params.map(p => (nTypeToType(p._1), nTypeToType(p._2))),
      nTypeToType(c.superType).asInstanceOf[RefType],
      c.fields.map(f => (nTypeToType(f._1),f._2)), c.methods.map(m => Method(freshTPV(), m.name, m.params.map(p => (freshTPV(), p)), m.retExpr))))

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
    private def nTypeToType(t : NType): Type = RefType(t.name, t.params.map(nTypeToType))
  }
}
