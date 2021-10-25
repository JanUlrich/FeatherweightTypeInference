package hb.dhbw


sealed abstract class UnifyConstraint(val left: Type, val right: Type)
final case class UnifyLessDot(override val left: Type, override val right: Type) extends UnifyConstraint(left, right)
final case class UnifyEqualsDot(override val left: Type, override val right: Type) extends UnifyConstraint(left, right)

object Unify {

  def unify(orCons: Set[Set[Set[UnifyConstraint]]], fc: FiniteClosure) : Set[Set[UnifyConstraint]] = {
    val eqSets = cartesianProduct(orCons)
    val step2Results = eqSets.flatMap(eqSet => {
      val rulesResult = applyRules(fc)(eqSet.flatten)
      step2(rulesResult, fc)
    })
    step2Results.flatMap(eqSet => {
      val (substResult, unifier) = substStep(eqSet)
      if(!unifier.isDefined){
        Set(substResult)
      }else{
        unify(Set(Set(substResult)), fc).map(s => s + unifier.get)
      }
    })
  }

  def step2(eq : Set[UnifyConstraint], fc: FiniteClosure) ={
    val eq1 = eq.filter(c => c match{
      case UnifyLessDot(TypeVariable(_), TypeVariable(_)) => true
      case UnifyEqualsDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    })
    val cUnifyLessDotACons: Set[Set[Set[UnifyConstraint]]] = eq.map(c => c match{
      case UnifyLessDot(RefType(name,params), TypeVariable(a)) =>
        fc.superTypes(RefType(name,params))
        .map(superType => Set(UnifyEqualsDot(TypeVariable(a), superType).asInstanceOf[UnifyConstraint]))
      case _ => null
    }).filter(s => s!=null)

    val aUnifyLessDota = eq1.filter(c => c match{
      case UnifyLessDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]

    val aUnifyLessDotCConsAndBs: Set[(UnifyLessDot,Option[TypeVariable])] = eq.map(c => c match{
      case UnifyLessDot(TypeVariable(a),RefType(name,params)) =>{
        val bs = aUnifyLessDota.flatMap(c => Set(c.left, c.right)).asInstanceOf[Set[TypeVariable]]
          .filter(c => !a.equals(c) && isLinked(TypeVariable(a), c, aUnifyLessDota))
        if(bs.isEmpty){
          Set((UnifyLessDot(TypeVariable(a),RefType(name,params)),None))
        }else{
          bs.map(b => (UnifyLessDot(TypeVariable(a),RefType(name,params)),Some(b)))
        }
      }
      case _ => null
    }).filter(s => s!=null).flatten

    val aUnifyLessDotCCons =  aUnifyLessDotCConsAndBs.map{
      case (ac:UnifyLessDot,Some(b)) =>
        Set(Set(UnifyLessDot(b, ac.right))) ++
          fc.superTypes(ac.right.asInstanceOf[RefType])
            .map(superType => Set(UnifyEqualsDot(b, superType)))
    case (ac, None) => null
    }.filter(c => c != null).asInstanceOf[Set[Set[Set[UnifyConstraint]]]]

    val eq2 = eq.filter(c => c match{
      case UnifyLessDot(TypeVariable(_), RefType(_,_)) => true
      case UnifyEqualsDot(TypeVariable(_), RefType(_,_)) => true
      case UnifyEqualsDot(RefType(_,_),TypeVariable(_)) => true
      case _ => false
    })
    val eqSet = cartesianProduct(Set(Set(eq1)) ++ Set(Set(eq2)) ++ aUnifyLessDotCCons ++ cUnifyLessDotACons)
    eqSet.map( s => s.flatten)
  }

  private def getAUnifyLessDotC(from: Set[UnifyConstraint]) = from.filter(c => c match{
    case UnifyLessDot(TypeVariable(_), RefType(_,_)) => true
    case _ => false
  }).asInstanceOf[Set[UnifyLessDot]]

  def matchRule(eq : Set[UnifyConstraint], fc: FiniteClosure) = {
    val aUnifyLessDotC = getAUnifyLessDotC(eq)
    (eq -- aUnifyLessDotC) ++ aUnifyLessDotC.map(c => {
      val smallerC = aUnifyLessDotC.find(c2 => c2 != c && c2.left.equals(c.left) && fc.isPossibleSupertype(c2.right.asInstanceOf[RefType].name,c.right.asInstanceOf[RefType].name))
      if(smallerC.isEmpty){
        c
      }else{
        UnifyLessDot(smallerC.get.right, c.right)
      }
    }
    )
  }

  def reduceRule(eq: Set[UnifyConstraint]) =  eq.flatMap(c => c match {
    case UnifyEqualsDot(RefType(an, ap), RefType(bn, bp)) => {
      if(an.equals(bn)){
        ap.zip(bp).map(p => UnifyEqualsDot(p._1, p._2))
      }else{
        Set(UnifyLessDot(RefType(an, ap), RefType(bn, bp)))
      }
    }
    case x => Set(x)
  })

  def swapRule(eq : Set[UnifyConstraint]) = eq.map(c => c match {
    case UnifyEqualsDot(RefType(an, ap), TypeVariable(a)) => UnifyEqualsDot(TypeVariable(a), RefType(an, ap))
    case x => x
  })

  def adaptRule(eq: Set[UnifyConstraint], fc: FiniteClosure) = {
    eq.map(c => c match {
      case UnifyLessDot(RefType(an, ap), RefType(bn, bp)) => {
        if(fc.isPossibleSupertype(an, bn)){
          UnifyEqualsDot(fc.superTypes(RefType(an, ap)).find(r => r.name.equals(bn)).get, RefType(bn, bp))
        }else{
          UnifyLessDot(RefType(an, ap), RefType(bn, bp))
        }
      }
      case x => x
    })
  }

  def adoptRule(eq: Set[UnifyConstraint], fc: FiniteClosure) ={
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]
    val aUnifyLessDotC = getAUnifyLessDotC(eq)
    (eq -- aUnifyLessDotC) ++ aUnifyLessDotC.map(c => {
      val smallerC = aUnifyLessDotC.find(c2 => c2 != c
        && isLinked(c2.left.asInstanceOf[TypeVariable], c.left.asInstanceOf[TypeVariable], aUnifyLessDota)
      && fc.isPossibleSupertype(c2.right.asInstanceOf[RefType].name,c.right.asInstanceOf[RefType].name))
      if(smallerC.isEmpty){
        c
      }else{
        UnifyLessDot(smallerC.get.right, c.right)
      }
    }
    )
  }

  private def isLinked(a: TypeVariable, b: TypeVariable, aUnifyLessDota: Set[UnifyLessDot]): Boolean = {
    def getRightSides(of: TypeVariable) ={
      aUnifyLessDota.filter(c => c.left.asInstanceOf[TypeVariable].name.equals(of.name))
    }
    val rightsides = getRightSides(a).map(c => c.right)
    if(rightsides.isEmpty){
      false
    } else if (rightsides.contains(b)){
      true
    }else{
      rightsides.foldLeft(false)((r, c) => r || isLinked(c.asInstanceOf[TypeVariable],b, aUnifyLessDota))
    }
  }

  private def findCircles(aUnifyLessDota: Set[UnifyLessDot]) ={
    def getRightSides(of: TypeVariable) ={
      aUnifyLessDota.filter(c => c.left.asInstanceOf[TypeVariable].name.equals(of.name))
    }
    def findCircle(graph: List[UnifyLessDot]): List[UnifyLessDot] = {
      val newAdditions = getRightSides(graph.last.right.asInstanceOf[TypeVariable])
      var circle: List[UnifyLessDot] = List()
      val iterator = newAdditions.iterator
      while(iterator.hasNext && circle.isEmpty){
        val newAdd = iterator.next()
        if(newAdd.right.equals(graph.head.left)){
          circle = graph ++ List(newAdd)
        }else{
          circle = findCircle(graph ++ List(newAdd))
        }
      }
      circle
    }
    aUnifyLessDota.view.map(c => findCircle(List(c)))
  }

  def equalsRule(eq: Set[UnifyConstraint]) ={
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]
    val circle = findCircles(aUnifyLessDota).find(!_.isEmpty)
    if(circle.isDefined){
      val newEq = eq -- circle.get
      Some(newEq ++ (circle.get.map(c => UnifyEqualsDot(c.left, c.right))))
    }else{
      None
    }
  }

  private def paramsContain(tv: TypeVariable, inParams: RefType): Boolean =
    inParams.params.find(t => t match {
      case TypeVariable(a) => tv.equals(TypeVariable(a))
      case RefType(a,p) => paramsContain(tv, RefType(a,p))
    }).isDefined
  def substStep(eq: Set[UnifyConstraint]) =  eq.find(c => c match {
    case UnifyEqualsDot(TypeVariable(a), RefType(n, p)) => !paramsContain(TypeVariable(a), RefType(n,p))
    case UnifyEqualsDot(TypeVariable(a), TypeVariable(b)) => true
    case _ => false
  }).map(c => (subst(c.left.asInstanceOf[TypeVariable], c.right, eq), Some(c))).getOrElse((eq, None))

  private def substHelper(a: TypeVariable, withType: Type,in: Type) :Type = in match {
    case RefType(n, p) => RefType(n,p.map(t => substHelper(a, withType, t)).asInstanceOf[List[Type]])
    case TypeVariable(n) =>
      if(a.equals(in)){withType}else{in}
  }

  def subst(a: TypeVariable, substType: Type,eq: Set[UnifyConstraint]): Set[UnifyConstraint] = {
    eq.map(c => c match {
      case UnifyLessDot(left, right) => UnifyLessDot(substHelper(a, substType, left), substHelper(a, substType, right))
      case UnifyEqualsDot(left, right) => UnifyEqualsDot(substHelper(a, substType, left), substHelper(a, substType, right))
    })
  }

  private def doWhileSome(fun: Set[UnifyConstraint]=>Option[Set[UnifyConstraint]], eqTemp: Set[UnifyConstraint]): Set[UnifyConstraint] =
    fun(eqTemp).map(eqTemp2 => doWhileSome(fun,eqTemp2)).getOrElse(eqTemp)

  def applyRules(fc: FiniteClosure) = (eq: Set[UnifyConstraint]) => {
    var eqNew: Set[UnifyConstraint] = null
    var eqFinish: Set[UnifyConstraint] = eq
    do{
      eqNew = doWhileSome(Unify.equalsRule,eqFinish)
      eqFinish = reduceRule(matchRule(adaptRule(adaptRule(eqNew, fc), fc), fc))
    }while(!eqNew.equals(eqFinish))
    eqNew
  }

  def cartesianProduct[T](xss: Set[Set[T]]): Set[Set[T]] =
    if(xss.isEmpty){
      Set(Set())
    } else{
      for(xh <- xss.head; xt <- cartesianProduct(xss.tail)) yield xt + xh
    }


}

