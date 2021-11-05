package hb.dhbw

class InsertTypes {

  def insert(unifyResult: Set[Set[UnifyConstraint]], into: Class): Class = {
    val constraints = unifyResult.map(_.map(replaceTVWithGeneric(_)))
    val newMethods = into.methods.flatMap(m => constraints.map(cons => insert(cons, m)))
    Class(into.name, into.genericParams, into.superType, into.fields, newMethods)
  }

  private def insert(constraints: Set[Constraint], into: Method): Method = {
    Method(into.genericParams ++ constraints, into.retType, into.name, into.params, into.retExpr)
  }
  private def replaceTVWithGeneric(in: UnifyConstraint): Constraint= in match {
    case UnifyLessDot(a,b) => LessDot(replaceTVWithGeneric(a), replaceTVWithGeneric(b))
    case UnifyEqualsDot(a, b) => EqualsDot(replaceTVWithGeneric(a), replaceTVWithGeneric(b))
  }
  private def replaceTVWithGeneric(in: UnifyType) : Type = in match {
    case UnifyRefType(name, params) => RefType(name, params.map(replaceTVWithGeneric(_)))
    case UnifyTV(name) => GenericType(name)
  }

}
