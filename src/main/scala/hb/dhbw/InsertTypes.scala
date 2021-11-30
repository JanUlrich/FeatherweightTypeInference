package hb.dhbw

object InsertTypes {

  /**
   * Remove a <. b constraints
   * @param eq
   * @return
   */
  private def flatten(eq:Set[UnifyConstraint]) = {
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
    ret ++ alessdotB.map(cons => UnifyEqualsDot(cons.left, cons.right))
  }


  def insert(unifyResult: Set[Set[UnifyConstraint]], into: Class): Class = {

    val flatted = unifyResult.map(flatten(_))

    /**
     * We have to also replace Reftypes with Generic variables
     * The unify algorithm does not know generic types and uses UnifyRefType for every type
     * we have to reverse that
     */
    val genericNames:Set[String] = into.genericParams.map(_._1).flatMap(_ match {
      case GenericType(name) => Some(name)
      case _ => None
    }).toSet
    def convertFromUnifyConstraint(t: UnifyType): Type = t match {
      case UnifyTV(a) => TypeVariable(a)
      case UnifyRefType(n, List()) => if(genericNames.contains(n)) GenericType(n) else RefType(n, List())
      case UnifyRefType(n, params) => RefType(n, params.map(convertFromUnifyConstraint(_)))
    }
    def convertFromUnifyConstraint(c: UnifyConstraint): Constraint = c match {
      case UnifyLessDot(a, b) => LessDot(convertFromUnifyConstraint(a), convertFromUnifyConstraint(b))
    }
    def convertFromUnifyConstraint(cons: Set[UnifyConstraint]): Set[Constraint] = cons.map(convertFromUnifyConstraint(_))
  // TODO
    val constraints = flatted.map(convertFromUnifyConstraint(_))

    /*

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
    val constraints = normalized.map(_.map(replaceRefTypeWithGeneric(_, genericNames)))
 */
    val newMethods = into.methods.flatMap(m => constraints.map(cons => insert(cons, m)))
    Class(into.name, into.genericParams, into.superType, into.fields, newMethods)
  }

  private def getLinkedConstraints(linkedTypes: Set[Type], in: Set[Constraint]): Set[Constraint] ={
    var typesWithoutBounds = linkedTypes
    in.flatMap(_ match {
      case LessDot(GenericType(a), RefType(name, params)) => {
        if(linkedTypes.contains(GenericType(a))){
          typesWithoutBounds = typesWithoutBounds - GenericType(a)
          val genericsInParams = params.filter(_ match {
            case GenericType(_) => true
            case _ => false
          }).toSet
          getLinkedConstraints(genericsInParams, in) +LessDot(GenericType(a), RefType(name, params))
        }else{
          Set()
        }
      }
      case _ => Set()
    }) ++ typesWithoutBounds.map(t => LessDot(t, RefType("Object", List())))
  }

  private def insert(constraints: Set[Constraint], into: Method): Method = {
    def replaceTVWithGeneric(in: Type): Type = in match {
      case TypeVariable(name) => GenericType(name)
      case RefType(name, params) => RefType(name, params.map(replaceTVWithGeneric(_)))
    }
    def substType(t: Type) = constraints.map(_ match {
      case EqualsDot(t1, t2) => if(t.equals(t1)) t2 else null
      case _ => null
    }).find(_ != null)
      .map(replaceTVWithGeneric(_))
      .getOrElse(if(t.isInstanceOf[TypeVariable]) GenericType(t.asInstanceOf[TypeVariable].name) else t)

    def getAllGenerics(from: Type): Set[Type] = from match {
      case RefType(name, params) => params.flatMap(getAllGenerics(_)).toSet
      case GenericType(a) => Set(GenericType(a))
    }

    val genericRetType = substType(into.retType)
    val genericParams = into.params.map(p => (substType(p._1), p._2))
    val tvsUsedInMethod = (Set(genericRetType) ++ genericParams.map(_._1)).flatMap(getAllGenerics(_))
    val constraintsForMethod = getLinkedConstraints(tvsUsedInMethod, constraints)

    Method(into.genericParams ++ constraintsForMethod, genericRetType, into.name, genericParams, into.retExpr)
  }

  private def replaceRefTypeWithGeneric(in: UnifyConstraint, genericNames: Set[String]): Constraint= in match {
    case UnifyLessDot(a,b) => LessDot(replaceRefTypeWithGeneric(a, genericNames), replaceRefTypeWithGeneric(b, genericNames))
    case UnifyEqualsDot(a, b) => EqualsDot(replaceRefTypeWithGeneric(a, genericNames), replaceRefTypeWithGeneric(b, genericNames))
  }
  private def replaceRefTypeWithGeneric(in: UnifyType, genericNames: Set[String]) : Type = in match {
    case UnifyRefType(name, List()) => if(genericNames.contains(name)) GenericType(name) else RefType(name,List())
    case UnifyRefType(name, params) => RefType(name, params.map(replaceRefTypeWithGeneric(_, genericNames)))
    case UnifyTV(name) => GenericType(name)
  }

}
