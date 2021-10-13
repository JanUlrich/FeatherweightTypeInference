package hb.dhbw


object Unify {

  sealed abstract class Constraint(val left: Type, val right: Type)
  final case class LessDot(override val left: Type, override val right: Type) extends Constraint(left, right)
  final case class EqualsDot(override val left: Type, override val right: Type) extends Constraint(left, right)

  def unify(orCons: Set[Set[Set[Constraint]]], fc: FiniteClosure) : Set[Set[Constraint]] = {
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

  def step2(eq : Set[Constraint], fc: FiniteClosure) ={
    val eq1 = eq.filter(c => c match{
      case LessDot(TypeVariable(_), TypeVariable(_)) => true
      case EqualsDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    })
    val cLessdotACons: Set[Set[Set[Constraint]]] = eq.map(c => c match{
      case LessDot(RefType(name,params), TypeVariable(a)) =>
        fc.superTypes(RefType(name,params))
        .map(superType => Set(EqualsDot(TypeVariable(a), superType).asInstanceOf[Constraint]))
      case _ => null
    }).filter(s => s!=null)

    val alessdota = eq1.filter(c => c match{
      case LessDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    }).asInstanceOf[Set[LessDot]]

    val aLessdotCConsAndBs: Set[(LessDot,Option[TypeVariable])] = eq.map(c => c match{
      case LessDot(TypeVariable(a),RefType(name,params)) =>{
        val bs = alessdota.flatMap(c => Set(c.left, c.right)).asInstanceOf[Set[TypeVariable]]
          .filter(c => !a.equals(c) && isLinked(TypeVariable(a), c, alessdota))
        if(bs.isEmpty){
          Set((LessDot(TypeVariable(a),RefType(name,params)),None))
        }else{
          bs.map(b => (LessDot(TypeVariable(a),RefType(name,params)),Some(b)))
        }
      }
      case _ => null
    }).filter(s => s!=null).flatten

    val aLessdotCCons =  aLessdotCConsAndBs.map{
      case (ac:LessDot,Some(b)) =>
        Set(Set(LessDot(b, ac.right))) ++
          fc.superTypes(ac.right.asInstanceOf[RefType])
            .map(superType => Set(EqualsDot(b, superType)))
    case (ac, None) => null
    }.filter(c => c != null).asInstanceOf[Set[Set[Set[Constraint]]]]

    val eq2 = eq.filter(c => c match{
      case LessDot(TypeVariable(_), RefType(_,_)) => true
      case EqualsDot(TypeVariable(_), RefType(_,_)) => true
      case EqualsDot(RefType(_,_),TypeVariable(_)) => true
      case _ => false
    })
    val eqSet = cartesianProduct(Set(Set(eq1)) ++ Set(Set(eq2)) ++ aLessdotCCons ++ cLessdotACons)
    eqSet.map( s => s.flatten)
  }

  private def getALessdotC(from: Set[Constraint]) = from.filter(c => c match{
    case LessDot(TypeVariable(_), RefType(_,_)) => true
    case _ => false
  }).asInstanceOf[Set[LessDot]]

  def matchRule(eq : Set[Constraint], fc: FiniteClosure) = {
    val aLessdotC = getALessdotC(eq)
    (eq -- aLessdotC) ++ aLessdotC.map(c => {
      val smallerC = aLessdotC.find(c2 => c2 != c && c2.left.equals(c.left) && fc.isPossibleSupertype(c2.right.asInstanceOf[RefType].name,c.right.asInstanceOf[RefType].name))
      if(smallerC.isEmpty){
        c
      }else{
        LessDot(smallerC.get.right, c.right)
      }
    }
    )
  }

  def reduceRule(eq: Set[Constraint]) =  eq.flatMap(c => c match {
    case EqualsDot(RefType(an, ap), RefType(bn, bp)) => {
      if(an.equals(bn)){
        ap.zip(bp).map(p => EqualsDot(p._1, p._2))
      }else{
        Set(LessDot(RefType(an, ap), RefType(bn, bp)))
      }
    }
    case x => Set(x)
  })

  def swapRule(eq : Set[Constraint]) = eq.map(c => c match {
    case EqualsDot(RefType(an, ap), TypeVariable(a)) => EqualsDot(TypeVariable(a), RefType(an, ap))
    case x => x
  })

  def adaptRule(eq: Set[Constraint], fc: FiniteClosure) = {
    eq.map(c => c match {
      case LessDot(RefType(an, ap), RefType(bn, bp)) => {
        if(fc.isPossibleSupertype(an, bn)){
          EqualsDot(fc.superTypes(RefType(an, ap)).find(r => r.name.equals(bn)).get, RefType(bn, bp))
        }else{
          LessDot(RefType(an, ap), RefType(bn, bp))
        }
      }
      case x => x
    })
  }

  def adoptRule(eq: Set[Constraint], fc: FiniteClosure) ={
    val aLessdota = eq.filter(c => c match{
      case LessDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    }).asInstanceOf[Set[LessDot]]
    val aLessdotC = getALessdotC(eq)
    (eq -- aLessdotC) ++ aLessdotC.map(c => {
      val smallerC = aLessdotC.find(c2 => c2 != c
        && isLinked(c2.left.asInstanceOf[TypeVariable], c.left.asInstanceOf[TypeVariable], aLessdota)
      && fc.isPossibleSupertype(c2.right.asInstanceOf[RefType].name,c.right.asInstanceOf[RefType].name))
      if(smallerC.isEmpty){
        c
      }else{
        LessDot(smallerC.get.right, c.right)
      }
    }
    )
  }

  private def isLinked(a: TypeVariable, b: TypeVariable, aLessdota: Set[LessDot]): Boolean = {
    def getRightSides(of: TypeVariable) ={
      aLessdota.filter(c => c.left.asInstanceOf[TypeVariable].name.equals(of.name))
    }
    val rightsides = getRightSides(a).map(c => c.right)
    if(rightsides.isEmpty){
      false
    } else if (rightsides.contains(b)){
      true
    }else{
      rightsides.foldLeft(false)((r, c) => r || isLinked(c.asInstanceOf[TypeVariable],b, aLessdota))
    }
  }

  private def findCircles(aLessdota: Set[LessDot]) ={
    def getRightSides(of: TypeVariable) ={
      aLessdota.filter(c => c.left.asInstanceOf[TypeVariable].name.equals(of.name))
    }
    def findCircle(graph: List[LessDot]): List[LessDot] = {
      val newAdditions = getRightSides(graph.last.right.asInstanceOf[TypeVariable])
      var circle: List[LessDot] = List()
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
    aLessdota.view.map(c => findCircle(List(c)))
  }

  def equalsRule(eq: Set[Constraint]) ={
    val aLessdota = eq.filter(c => c match{
      case LessDot(TypeVariable(_), TypeVariable(_)) => true
      case _ => false
    }).asInstanceOf[Set[LessDot]]
    val circle = findCircles(aLessdota).find(!_.isEmpty)
    if(circle.isDefined){
      val newEq = eq -- circle.get
      Some(newEq ++ (circle.get.map(c => EqualsDot(c.left, c.right))))
    }else{
      None
    }
  }

  private def paramsContain(tv: TypeVariable, inParams: RefType): Boolean =
    inParams.params.find(t => t match {
      case TypeVariable(a) => tv.equals(TypeVariable(a))
      case RefType(a,p) => paramsContain(tv, RefType(a,p))
    }).isDefined
  def substStep(eq: Set[Constraint]) =  eq.find(c => c match {
    case EqualsDot(TypeVariable(a), RefType(n, p)) => !paramsContain(TypeVariable(a), RefType(n,p))
    case _ => false
  }).map(c => (subst(c.left.asInstanceOf[TypeVariable], c.right.asInstanceOf[RefType], eq), Some(c))).getOrElse((eq, None))

  private def subst(a: TypeVariable, withType: RefType,in: Type) :Type = in match {
    case RefType(n, p) => RefType(n,p.map(t => subst(a, withType, t)).asInstanceOf[List[Type]])
    case TypeVariable(n) =>
      if(a.equals(in)){withType}else{in}
  }

  def subst(a: TypeVariable, refType: RefType,eq: Set[Constraint]): Set[Constraint] = {
    eq.map(c => c match {
      case LessDot(left, right) => LessDot(subst(a, refType, left), subst(a, refType, right))
      case EqualsDot(left, right) => EqualsDot(subst(a, refType, left), subst(a, refType, right))
    })
  }

  private def doWhileSome(fun: Set[Constraint]=>Option[Set[Constraint]], eqTemp: Set[Constraint]): Set[Constraint] =
    fun(eqTemp).map(eqTemp2 => doWhileSome(fun,eqTemp2)).getOrElse(eqTemp)

  def applyRules(fc: FiniteClosure) = (eq: Set[Constraint]) => {
    var eqNew: Set[Constraint] = null
    var eqFinish: Set[Constraint] = eq
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

