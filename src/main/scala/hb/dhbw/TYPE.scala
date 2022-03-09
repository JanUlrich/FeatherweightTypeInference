package hb.dhbw


sealed trait Constraint
final case class AndConstraint(andCons: List[Constraint]) extends Constraint
final case class OrConstraint(orCons: List[Constraint]) extends Constraint
final case class EqualsDot(l: Type, r: Type) extends Constraint
final case class LessDot(l: Type, r: Type) extends Constraint

object TYPE {
  def generateConstraints(c: List[Class], finiteClosure: FiniteClosure) = {
    new TYPEMonad().TYPEClass(c.last, c, finiteClosure)
  }

  private class GenericTypeReplaceMonad(tpvs: TYPEMonad){
    var genericNameToTVMap: Map[String, TypeVariable] = Map()

    def replaceGenerics(inConstraints: List[Constraint]): List[Constraint] = inConstraints.map(replaceGenerics(_))

    def replaceGenerics(inConstraint: Constraint): Constraint = inConstraint match {
      case OrConstraint(cons) => OrConstraint(cons.map(replaceGenerics(_)))
      case AndConstraint(andCons) => AndConstraint(andCons.map(replaceGenerics(_)))
      case LessDot(l, r) => LessDot(replaceGenerics(l), replaceGenerics(r))
      case EqualsDot(l, r) => EqualsDot(replaceGenerics(l), replaceGenerics(r))
    }
    def replaceGenerics(inType: Type): Type= inType match {
      case RefType(name, params) =>RefType(name, params.map(replaceGenerics(_)))
      case GenericType(name) => genericNameToTVMap.get(name)
        .getOrElse{
          val newTV = tpvs.freshTPV()
          genericNameToTVMap = genericNameToTVMap + (name -> newTV)
          newTV
        }
      case x => x
    }
  }

  private class TYPEMonad{
    var tpvNum = 0

    def TYPEClass(currentClass: Class, ast: List[Class], fc: FiniteClosure) = {
      ({
        val thisType = RefType(currentClass.name, currentClass.genericParams.map(it => RefType(it._1.asInstanceOf[GenericType].name, List())))
        currentClass.methods.flatMap(m => TYPEMethod(m, thisType, ast))
      }, fc)
    }

    def freshTPV() = {
      tpvNum = tpvNum+1
      TypeVariable(tpvNum.toString)
    }

    private def TYPEMethod(method: Method, thisType: RefType, ast: List[Class]) = {
      val (rty, cons) = TYPEExpr(method.retExpr, List((thisType, "this")) ++ method.params, ast)
      LessDot(rty, method.retType) :: cons
    }

    private def TYPEExpr(expr: Expr, localVars: List[(Type, String)], ast: List[Class]) : (Type, List[Constraint]) =expr match {
      case LocalVar(n) => localVars.find(it => it._2.equals(n)).map(p => (p._1, List()))
        .getOrElse(throw new Exception("Local Variable "+ n + " not found"))
      case FieldVar(e, f) => {
        val genericReplace = new GenericTypeReplaceMonad(this)
        val (rty, cons) = TYPEExpr(e, localVars, ast)
        val fields = findFields(f, ast)
        val a = freshTPV()
        val orCons = OrConstraint(fields.map(f => AndConstraint(List(EqualsDot(rty, cToType(f._1)), EqualsDot(a, f._2)))))
        (a, genericReplace.replaceGenerics(orCons :: cons))
      }
      case MethodCall(e, name, params) => {
        val genericReplace = new GenericTypeReplaceMonad(this)
        val a = freshTPV()
        val (rty, cons) = TYPEExpr(e, localVars, ast)
        val es = params.map(ex => TYPEExpr(ex, localVars, ast))
        val methods = findMethods(name, es.size, ast)
        val consM = methods.map(m => AndConstraint(m._2.genericParams ++
          List(EqualsDot(rty, cToType(m._1)), EqualsDot(a, m._2.retType))
          ++ m._2.params.map(_._1).zip(es.map(_._1)).map(a => LessDot(a._2, a._1))
        ))
        val retCons = (cons ++ es.flatMap(_._2) ++ List(OrConstraint(consM)))
        (a, genericReplace.replaceGenerics(retCons))
      }
      case Constructor(className, params) => {
        val genericReplace = new GenericTypeReplaceMonad(this)
        val es = params.map(ex => TYPEExpr(ex, localVars, ast))
        val cl = findClasses(className, ast)
        val paramCons = cl.fields.map(_._1).zip(es.map(_._1)).map(pair => LessDot(pair._2, pair._1))
        val retCons = paramCons ++ es.flatMap(_._2) ++
          cl.genericParams.map(gp => LessDot(gp._1, gp._2))
        (RefType(className, cl.genericParams.map(_._1).map(genericReplace.replaceGenerics(_))), genericReplace.replaceGenerics(retCons))
      }
      case Cast(casttype, expr) => {
        val (rty, cons) = TYPEExpr(expr, localVars, ast)
        (casttype, cons)
      }
    }

    private def findMethods(m: String, numParams: Int, ast: List[Class]) =
      ast.flatMap(c => c.methods.filter(method => method.name.equals(m) && method.params.size == numParams).map(it => (c, it)))

    private def findFields(f: String, ast: List[Class]) =
      ast.flatMap(c => c.fields.filter(field => field._2.equals(f)).map(it => (c, it._1)))

    def findClasses(className: String, ast: List[Class]) = ast.filter(c => c.name.equals(className)).head

    private def cToType(c: Class) = RefType(c.name, c.genericParams.map(it => it._1))

  }
}
