package hb.dhbw


sealed abstract class UnifyConstraint(val left: UnifyType, val right: UnifyType)
final case class UnifyLessDot(override val left: UnifyType, override val right: UnifyType) extends UnifyConstraint(left, right)
final case class UnifyEqualsDot(override val left: UnifyType, override val right: UnifyType) extends UnifyConstraint(left, right)

sealed abstract class UnifyType
final case class UnifyRefType(name: String, params: List[UnifyType]) extends UnifyType
final case class UnifyTV(name: String) extends UnifyType

/*
sealed abstract class ResultType
final case class ResultTV(name: String) extends ResultType
final case class ResultRefType(name: String, params: List[ResultType]) extends ResultType
sealed abstract class UnifyResultConstraint
final case class AExtendsB(a: TypeVariable, b: TypeVariable) extends UnifyResultConstraint
final case class AExtendsN(a: TypeVariable, n: ResultRefType) extends UnifyResultConstraint
final case class AEqualsB(a: TypeVariable, b: TypeVariable) extends UnifyResultConstraint
final case class AEqualsN(a: TypeVariable, n: ResultRefType) extends UnifyResultConstraint
*/

object Unify {

  sealed trait Step4Result{
    def hasChanged(): Boolean
    def result: Set[UnifyConstraint]
  }
  final case class ChangedSet(eq: Set[UnifyConstraint]) extends Step4Result {
    override def hasChanged(): Boolean = true
    override def result: Set[UnifyConstraint] = eq
  }
  final case class UnchangedSet(eq: Set[UnifyConstraint]) extends Step4Result {
    override def hasChanged(): Boolean = false
    override def result: Set[UnifyConstraint] = eq
  }

  def unifyIterative(orCons: Set[Set[Set[UnifyConstraint]]], fc: FiniteClosure) : Set[Set[UnifyConstraint]] = {
    def getNext[A](from: Set[CartesianProduct[A]])=
      from.find(_.hasNext()).map(it => it.nextProduct())

    var eqSets: Set[CartesianProduct[Set[UnifyConstraint]]] =
      Set(new CartesianProduct[Set[UnifyConstraint]](orCons))
    var it: Option[Set[Set[UnifyConstraint]]] = getNext(eqSets)
    var results: Set[Set[UnifyConstraint]] = Set()
    while(it.isDefined){
      val eqSet = it.get
      val rulesResult = applyRules(fc)(eqSet.flatten)
      val step2Result = step2(rulesResult, fc)
      while(step2Result.hasNext()){
        val substResult = substStep(step2Result.nextProduct().flatten)
        substResult match{
          case UnchangedSet(eq) => if(isSolvedForm(eq)){
            results = results + eq
          }
          case ChangedSet(eq) =>
            eqSets = eqSets + new CartesianProduct[Set[UnifyConstraint]](Set(Set(eq)))
        }
      }
      it = getNext(eqSets)
    }
    results
  }

  /*
  def unify(orCons: Set[Set[Set[UnifyConstraint]]], fc: FiniteClosure) : Set[Set[UnifyConstraint]] = {
    val eqSets = cartesianProduct(orCons)
    val step2Results = eqSets.flatMap(eqSet => {
      val rulesResult = applyRules(fc)(eqSet.flatten)
      step2(rulesResult, fc)
    })
    step2Results.flatMap(eqSet => {
      val (substResult, unifier) = substStep(eqSet)
      if(!unifier.isDefined){
        if(isSolvedForm(substResult))
          Set(substResult)
        else Set()
      }else{
        unify(Set(Set(substResult)), fc).map(s => s + unifier.get)
      }
    })
  }
*/
  def step2(eq : Set[UnifyConstraint], fc: FiniteClosure) ={
    val eq1 = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case UnifyEqualsDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    })
    val cUnifyLessDotACons: Set[Set[Set[UnifyConstraint]]] = eq.map(c => c match{
      case UnifyLessDot(UnifyRefType(name,params), UnifyTV(a)) =>
        getSuperTypes(UnifyRefType(name,params), fc)
          .map(superType => Set(UnifyEqualsDot(UnifyTV(a), superType).asInstanceOf[UnifyConstraint]))
      case _ => null
    }).filter(s => s!=null)

    val aUnifyLessDota = eq1.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]

    val aUnifyLessDotCConsAndBs: Set[(UnifyLessDot,Option[UnifyTV])] = eq.map(c => c match{
      case UnifyLessDot(UnifyTV(a),UnifyRefType(name,params)) =>{
        val bs = aUnifyLessDota.flatMap(c => Set(c.left, c.right)).asInstanceOf[Set[UnifyTV]]
          .filter(c => !a.equals(c) && isLinked(UnifyTV(a), c, aUnifyLessDota))
        if(bs.isEmpty){
          Set((UnifyLessDot(UnifyTV(a),UnifyRefType(name,params)),None))
        }else{
          bs.map(b => (UnifyLessDot(UnifyTV(a),UnifyRefType(name,params)),Some(b)))
        }
      }
      case _ => null
    }).filter(s => s!=null).flatten

    val aUnifyLessDotCCons =  aUnifyLessDotCConsAndBs.map{
      case (ac:UnifyLessDot,Some(b)) =>
        Set(Set(UnifyLessDot(b, ac.right))) ++
          getSuperTypes(ac.right.asInstanceOf[UnifyRefType], fc)
            .map(superType => Set(UnifyEqualsDot(b, superType)))
      case (ac, None) => null
    }.filter(c => c != null).asInstanceOf[Set[Set[Set[UnifyConstraint]]]]

    val eq2 = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyRefType(_,_)) => true
      case UnifyEqualsDot(UnifyTV(_), UnifyRefType(_,_)) => true
      case UnifyEqualsDot(UnifyRefType(_,_),UnifyTV(_)) => true
      case UnifyEqualsDot(UnifyRefType(_,_),UnifyRefType(_,_)) => true
      case UnifyLessDot(UnifyRefType(_,_),UnifyRefType(_,_)) => true
      case _ => false
    })
    val eqSet = new CartesianProduct[Set[UnifyConstraint]](
      Set(Set(eq1)) ++ Set(Set(eq2)) ++ aUnifyLessDotCCons ++ cUnifyLessDotACons)
    eqSet
  }

  private def getAUnifyLessDotC(from: Set[UnifyConstraint]) = from.filter(c => c match{
    case UnifyLessDot(UnifyTV(_), UnifyRefType(_,_)) => true
    case _ => false
  }).asInstanceOf[Set[UnifyLessDot]]

  def matchRule(eq : Set[UnifyConstraint], fc: FiniteClosure) = {
    val aUnifyLessDotC = getAUnifyLessDotC(eq)
    (eq -- aUnifyLessDotC) ++ aUnifyLessDotC.map(c => {
      val smallerC = aUnifyLessDotC.find(c2 => c2 != c && c2.left.equals(c.left) && fc.isPossibleSupertype(c2.right.asInstanceOf[UnifyRefType].name,c.right.asInstanceOf[UnifyRefType].name))
      if(smallerC.isEmpty){
        c
      }else{
        UnifyLessDot(smallerC.get.right, c.right)
      }
    }
    )
  }

  def reduceRule(eq: Set[UnifyConstraint]) =  eq.flatMap(c => c match {
    case UnifyEqualsDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)) => {
      if(an.equals(bn)){
        ap.zip(bp).map(p => UnifyEqualsDot(p._1, p._2))
      }else{
        Set(UnifyEqualsDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)))
      }
    }
    case x => Set(x)
  })

  def swapRule(eq : Set[UnifyConstraint]) = eq.map(c => c match {
    case UnifyEqualsDot(UnifyRefType(an, ap), UnifyTV(a)) => UnifyEqualsDot(UnifyTV(a), UnifyRefType(an, ap))
    case x => x
  })

  private def convert(fjType: FJType): UnifyType = fjType match {
    case FJNamedType(n, p) => UnifyRefType(n, p.map(convert))
    case FJTypeVariable(n) => UnifyTV(n)
  }
  private def convertNamedType(fjType: FJNamedType): UnifyRefType = UnifyRefType(fjType.name, fjType.params.map(convert))
  private def convertRefType(unifyType: UnifyRefType): FJNamedType = FJNamedType(unifyType.name, unifyType.params.map(convert(_)))
  private def convert(unifyType: UnifyType): FJType = unifyType match {
    case UnifyRefType(n, p) => FJNamedType(n, p.map(convert(_)))
    case UnifyTV(n) => FJTypeVariable(n)
  }
  private def getSuperTypes(of: UnifyRefType, fc: FiniteClosure) = fc.superTypes(convertRefType(of)).map(convertNamedType)
  def adaptRule(eq: Set[UnifyConstraint], fc: FiniteClosure) = {
    eq.map(c => c match {
      case UnifyLessDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)) => {
        if(fc.isPossibleSupertype(an, bn)){
          UnifyEqualsDot(getSuperTypes(UnifyRefType(an, ap), fc).find(r => r.name.equals(bn)).get, UnifyRefType(bn, bp))
        }else{
          UnifyLessDot(UnifyRefType(an, ap), UnifyRefType(bn, bp))
        }
      }
      case x => x
    })
  }

  def adoptRule(eq: Set[UnifyConstraint], fc: FiniteClosure) ={
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]
    val aUnifyLessDotC = getAUnifyLessDotC(eq)
    (eq -- aUnifyLessDotC) ++ aUnifyLessDotC.map(c => {
      val smallerC = aUnifyLessDotC.find(c2 => c2 != c
        && isLinked(c2.left.asInstanceOf[UnifyTV], c.left.asInstanceOf[UnifyTV], aUnifyLessDota)
      && fc.isPossibleSupertype(c2.right.asInstanceOf[UnifyRefType].name,c.right.asInstanceOf[UnifyRefType].name))
      if(smallerC.isEmpty){
        c
      }else{
        UnifyLessDot(smallerC.get.right, c.right)
      }
    }
    )
  }

  def eraseRule(eq: Set[UnifyConstraint]) =
    eq.filter(_ match {
      case UnifyEqualsDot(UnifyTV(a), UnifyTV(b)) => ! a.equals(b)
      case _ => true
    })

  private def isLinked(a: UnifyTV, b: UnifyTV, aUnifyLessDota: Set[UnifyLessDot]): Boolean = {
    def getRightSides(of: UnifyTV) ={
      aUnifyLessDota.filter(c => c.left.asInstanceOf[UnifyTV].name.equals(of.name))
    }
    val rightsides = getRightSides(a).map(c => c.right)
    if(rightsides.isEmpty){
      false
    } else if (rightsides.contains(b)){
      true
    }else{
      rightsides.foldLeft(false)((r, c) => r || isLinked(c.asInstanceOf[UnifyTV],b, aUnifyLessDota))
    }
  }

  private def findCircles(aUnifyLessDota: Set[UnifyLessDot]) ={
    def getRightSides(of: UnifyTV) ={
      aUnifyLessDota.filter(c => c.left.asInstanceOf[UnifyTV].name.equals(of.name))
    }
    def findCircle(graph: List[UnifyLessDot]): List[UnifyLessDot] = {
      val newAdditions = getRightSides(graph.last.right.asInstanceOf[UnifyTV])
      var circle: List[UnifyLessDot] = List()
      val iterator = newAdditions.iterator
      while(iterator.hasNext && circle.isEmpty){
        val newAdd = iterator.next()
        if(newAdd.right.equals(graph.head.left)){
          circle = graph :+ newAdd
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
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
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

  private def paramsContain(tv: UnifyTV, inParams: UnifyRefType): Boolean =
    inParams.params.find(t => t match {
      case UnifyTV(a) => tv.equals(UnifyTV(a))
      case UnifyRefType(a,p) => paramsContain(tv, UnifyRefType(a,p))
    }).isDefined
  def substStep(eq: Set[UnifyConstraint]): Step4Result = {
    def substCall(eq: Set[UnifyConstraint]) = eq.find(c => c match {
      case UnifyEqualsDot(UnifyTV(a), UnifyRefType(n, p)) => !paramsContain(UnifyTV(a), UnifyRefType(n,p))
      case UnifyEqualsDot(UnifyTV(a), UnifyTV(b)) => !a.equals(b)
      case _ => false
    }).map(c => (subst(c.left.asInstanceOf[UnifyTV], c.right, eq.filter(!_.equals(c))), Some(c))).getOrElse((eq, None))

    var substResult: (Set[UnifyConstraint], Option[UnifyConstraint]) = (eq, None)
    var substVars: Set[UnifyConstraint] = Set()
    do{
      substResult = substCall(substResult._1)
      substResult._2.map(it => {
        substVars = subst(it.left.asInstanceOf[UnifyTV], it.right, substVars)
        substVars = substVars + it
      })
    }while(substResult._2.isDefined)
    val result = substResult._1 ++ substVars
    if(result.equals(eq))
      UnchangedSet(eq)
    else
      ChangedSet(substResult._1 ++ substVars)
  }

  private def substHelper(a: UnifyTV, withType: UnifyType,in: UnifyType) :UnifyType = in match {
    case UnifyRefType(n, p) => UnifyRefType(n,p.map(t => substHelper(a, withType, t)))
    case UnifyTV(n) =>
      if(a.name.equals(n)){withType}else{in}
  }

  def subst(a: UnifyTV, substType: UnifyType,eq: Set[UnifyConstraint]): Set[UnifyConstraint] = {
    eq.map(c => c match {
      case UnifyLessDot(left, right) => UnifyLessDot(substHelper(a, substType, left), substHelper(a, substType, right))
      case UnifyEqualsDot(left, right) => UnifyEqualsDot(substHelper(a, substType, left), substHelper(a, substType, right))
    })
  }

  def isSolvedForm(eq: Set[UnifyConstraint]) =
    eq.filter(_ match {
      case UnifyLessDot(UnifyTV(a), UnifyTV(b)) => true //isIsolatedTV(UnifyTV(a), eq) && isIsolatedTV(UnifyTV(b), eq)
      case UnifyLessDot(UnifyTV(a), UnifyRefType(n, params)) => !getAllTvs(UnifyRefType(n, params)).contains(UnifyTV(a))
      case UnifyEqualsDot(UnifyTV(a), UnifyRefType(n, params)) => !getAllTvs(UnifyRefType(n, params)).contains(UnifyTV(a))
      case UnifyEqualsDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).size == eq.size

  private def getAllTvs(in: UnifyType): Set[UnifyTV] = in match {
    case UnifyTV(a) => Set(UnifyTV(a))
    case UnifyRefType(_, params) => params.flatMap(getAllTvs(_)).toSet
  }
  def isIsolatedTV(tv: UnifyTV, eq: Set[UnifyConstraint]) = {
    val notIsolatedTVs: Set[UnifyTV] = eq.flatMap(_ match {
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => Set[UnifyTV]()
      case UnifyEqualsDot(UnifyTV(_), UnifyTV(_)) => Set[UnifyTV]()
      case UnifyLessDot(a,b) => getAllTvs(a) ++ getAllTvs(b)
      case UnifyEqualsDot(a,b) => getAllTvs(a) ++ getAllTvs(b)
    })
    !notIsolatedTVs.contains(tv)
  }

  private def doWhileSome(fun: Set[UnifyConstraint]=>Option[Set[UnifyConstraint]], eqTemp: Set[UnifyConstraint]): Set[UnifyConstraint] =
    fun(eqTemp).map(eqTemp2 => doWhileSome(fun,eqTemp2)).getOrElse(eqTemp)

  def applyRules(fc: FiniteClosure) = (eq: Set[UnifyConstraint]) => {
    var eqNew: Set[UnifyConstraint] = null
    var eqFinish: Set[UnifyConstraint] = eq
    do{
      eqNew = doWhileSome(Unify.equalsRule,eqFinish) //We have to apply equals rule first, to get rid of circles
      eqFinish = eraseRule(swapRule(reduceRule(matchRule(adoptRule(adaptRule(eqNew, fc), fc), fc))))
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

