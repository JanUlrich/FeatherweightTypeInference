package hb.dhbw

object InsertTypes {

  def normalize(eq:Set[UnifyConstraint]) = {
    def substHelper(a: UnifyTV, withType: UnifyType,in: UnifyType) :UnifyType = in match {
      case UnifyRefType(n, p) => UnifyRefType(n,p.map(t => substHelper(a, withType, t)))
      case UnifyTV(n) =>
        if(a.equals(in)){withType}else{in}
    }
    def subst(a: UnifyTV, substType: UnifyType,eq: Set[UnifyConstraint]): Set[UnifyConstraint] = {
      eq.map(c => c match {
        case UnifyLessDot(left, right) => UnifyLessDot(substHelper(a, substType, left), substHelper(a, substType, right))
        case UnifyEqualsDot(left, right) => UnifyEqualsDot(substHelper(a, substType, left), substHelper(a, substType, right))
      })
    }

    val alessdotB = eq.filter(_ match{
      case UnifyLessDot(UnifyTV(a), UnifyTV(b)) => true
      case _ => false
    })
    var ret = eq.filter(_ match{
      case UnifyLessDot(UnifyTV(a), UnifyTV(b)) => false
      case _ => true
    })
    alessdotB.foreach(it => ret = subst(it.left.asInstanceOf[UnifyTV], it.right, ret))
    ret
  }

  def insert(unifyResult: Set[Set[UnifyConstraint]], into: Class): Class = {

    val normalized = unifyResult.map(normalize(_))

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
    val constraints = normalized.map(_.map(replaceTVWithGeneric(_, genericNames)))
    val newMethods = into.methods.flatMap(m => constraints.map(cons => insert(cons, m)))
    Class(into.name, into.genericParams, into.superType, into.fields, newMethods)
  }

  private def insert(constraints: Set[Constraint], into: Method): Method = {
    def getAllGenericTypes(from: Constraint): Set[Type] = from match {
      case EqualsDot(a,b) => getAllGenerics(a) ++ getAllGenerics(b)
      case LessDot(a,b) => getAllGenerics(a) ++ getAllGenerics(b)
    }
    def getAllGenerics(from: Type): Set[Type] = from match {
      case RefType(name, params) => params.flatMap(getAllGenerics(_)).toSet
      case GenericType(a) => Set(GenericType(a))
      case _ => Set()
    }
    def getLinkedCons(linkedTypes: Set[Type], in: Set[Constraint]): Set[Constraint] ={
      val linkedCons = in.filter(it => getAllGenericTypes(it).exists(linkedTypes.contains(_)))
      val newLinkedTypes = linkedCons.flatMap(getAllGenericTypes(_))
      if(newLinkedTypes.equals(linkedTypes))
        linkedCons
      else
        getLinkedCons(newLinkedTypes, in)
    }
    def replaceTVWithGeneric(in: Type): Type = in match {
      case TypeVariable(name) => GenericType(name)
      case RefType(name, params) => RefType(name, params.map(replaceTVWithGeneric(_)))
    }
    val genericRetType = replaceTVWithGeneric(into.retType)
    val genericParams = into.params.map(p => (replaceTVWithGeneric(p._1), p._2))
    val constraintsForMethod = getLinkedCons(Set(genericRetType) ++ genericParams.map(_._1), constraints)
    Method(into.genericParams ++ constraintsForMethod, genericRetType, into.name, genericParams, into.retExpr)
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
