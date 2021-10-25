package hb.dhbw


sealed trait Constraint
final case class AndConstraint(andCons: List[Constraint]) extends Constraint
final case class OrConstraint(orCons: List[Constraint]) extends Constraint
final case class EqualsDot(l: Type, r: Type) extends Constraint
final case class LessDot(l: Type, r: Type) extends Constraint

object TYPE {
  def generateConstraints(c: List[Class]) = {
    new TYPEMonad().TYPEClass(c)
  }

  private class TYPEMonad{
    var tpvNum = 0

    private def generateFC(ast: List[Class]): FiniteClosure = new FiniteClosure(ast.map(c => (cToType(c), c.superType)).toSet)

    def TYPEClass(ast: List[Class]) = {
      (ast.flatMap(cl => cl.methods.flatMap(m => TYPEMethod(m, cToType(cl), ast))), generateFC(ast))
    }

    private def freshTPV() = {
      tpvNum = tpvNum+1
      TypeVariable(tpvNum.toString)
    }

    private def TYPEMethod(method: Method, thisType: RefType, ast: List[Class]) = {
      val (rty, cons) = TYPEExpr(method.retExpr, List((thisType, "this")) ++ method.params, ast)
      LessDot(rty, method.retType) :: cons
    }

    private def TYPEExpr(expr: Expr, localVars: List[(Type, String)],ast: List[Class]) : (Type, List[Constraint]) =expr match {
      case LocalVar(n) => localVars.find(it => it._2.equals(n)).map(p => (p._1, List()))
        .getOrElse(throw new Exception("Local Variable "+ n + " not found"))
      case FieldVar(e, f) => {
        val (rty, cons) = TYPEExpr(e, localVars, ast)
        val fields = findFields(f, ast)
        val a = freshTPV()
        val orCons = OrConstraint(fields.map(f => AndConstraint(List(EqualsDot(rty, cToType(f._1)), EqualsDot(a, f._2)))))
        (a, orCons :: cons)
      }
      case MethodCall(e, name, params) => {
        val a = freshTPV()
        val (rty, cons) = TYPEExpr(e, localVars, ast)
        val es = params.map(ex => TYPEExpr(ex, localVars, ast))
        val methods = findMethods(name, es.size, ast)
        val consM = methods.map(m => AndConstraint(
          List(EqualsDot(rty, cToType(m._1)), EqualsDot(a, m._2.retType))
          ++ m._2.params.map(_._1).zip(es.map(_._1)).map(a => LessDot(a._2, a._1))
        ))
        (a, cons ++ es.flatMap(_._2) ++ List(OrConstraint(consM)))
      }
      case Constructor(className, params) => {
        throw new NotImplementedError()
      }
    }

    private def findMethods(m: String, numParams: Int, ast: List[Class]) =
      ast.flatMap(c => c.methods.filter(method => method.name.equals(m) && method.params.size == numParams).map(it => (c, it)))

    private def findFields(f: String, ast: List[Class]) =
      ast.flatMap(c => c.fields.filter(field => field._2.equals(f)).map(it => (c, it._1)))

    private def cToType(c: Class) = RefType(c.name, c.params.map(it => it._1))

  }
}
