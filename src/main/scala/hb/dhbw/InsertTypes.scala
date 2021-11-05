package hb.dhbw

object InsertTypes {

  def insert(unifyResult: Set[Set[UnifyConstraint]], into: Class): Class = {

    def extractTVNames(unifyType: UnifyType): Set[String] = unifyType match {
      case UnifyTV(name) => Set(name)
      case UnifyRefType(_, params) => params.flatMap(extractTVNames(_)).toSet
    }

    val genericNames:Set[String] = into.genericParams.map(_._1).flatMap(_ match {
      case GenericType(name) => Some(name)
      case _ => None
    }).toSet ++ unifyResult.flatMap(_.flatMap(_ match{
      case UnifyLessDot(a,b) => Set(a, b)
      case UnifyEqualsDot(a,b) => Set(a,b)
    })).flatMap(extractTVNames(_))
    val constraints = unifyResult.map(_.map(replaceTVWithGeneric(_, genericNames)))
    val newMethods = into.methods.flatMap(m => constraints.map(cons => insert(cons, m)))
    Class(into.name, into.genericParams, into.superType, into.fields, newMethods)
  }

  private def insert(constraints: Set[Constraint], into: Method): Method = {
    def replaceTVWithGeneric(in: Type): Type = in match {
      case TypeVariable(name) => GenericType(name)
      case RefType(name, params) => RefType(name, params.map(replaceTVWithGeneric(_)))
    }
    Method(into.genericParams ++ constraints, replaceTVWithGeneric(into.retType), into.name, into.params.map(p => (replaceTVWithGeneric(p._1), p._2)), into.retExpr)
  }
  private def replaceTVWithGeneric(in: UnifyConstraint, genericNames: Set[String]): Constraint= in match {
    case UnifyLessDot(a,b) => LessDot(replaceTVWithGeneric(a, genericNames), replaceTVWithGeneric(b, genericNames))
    case UnifyEqualsDot(a, b) => EqualsDot(replaceTVWithGeneric(a, genericNames), replaceTVWithGeneric(b, genericNames))
  }
  private def replaceTVWithGeneric(in: UnifyType, genericNames: Set[String]) : Type = in match {
    case UnifyRefType(name, List()) => if(genericNames.contains(name)) GenericType(name) else RefType(name,List())
    case UnifyRefType(name, params) => RefType(name, params.map(replaceTVWithGeneric(_, genericNames)))
    case UnifyTV(name) => GenericType(name)
  }

}
