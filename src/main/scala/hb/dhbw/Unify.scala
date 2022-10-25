package hb.dhbw

import scala.collection.mutable

sealed abstract class UnifyConstraint(val left: UnifyType, val right: UnifyType)
final case class UnifyLessDot(override val left: UnifyType, override val right: UnifyType) extends UnifyConstraint(left, right)
final case class UnifyEqualsDot(override val left: UnifyType, override val right: UnifyType) extends UnifyConstraint(left, right)

sealed abstract class UnifyType
final case class UnifyRefType(name: String, params: List[UnifyType]) extends UnifyType
final case class UnifyTV(name: String) extends UnifyType
final case class UnifyWildcard(name: String, upperBound: UnifyType, lowerBound: UnifyType) extends UnifyType

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

  def postProcessing(eq: Set[UnifyConstraint]): Set[UnifyConstraint] = {
    var ret = eq
    var ruleResult = subElimRule(ret)
    while(ruleResult.isDefined){
      ret = ruleResult.get
      ruleResult = subElimRule(ruleResult.get)
    }
    ret
  }

  def subElimRule(eq: Set[UnifyConstraint]) : Option[Set[UnifyConstraint]] = {
    eq.find(_ match{
      case UnifyLessDot(UnifyTV(a), UnifyTV(b)) => true
      case _ => false
    }).map(it => {
      subst(it.right.asInstanceOf[UnifyTV], it.left, eq.filter(it != _)) ++ Set(UnifyEqualsDot(it.right, it.left), UnifyEqualsDot(it.left, it.left))
    })
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
            results = results + postProcessing(eq)
          }
          case ChangedSet(eq) =>
            eqSets = eqSets + new CartesianProduct[Set[UnifyConstraint]](Set(Set(eq)))
        }
      }
      it = getNext(eqSets)
    }
    results
  }

  var tpvNum: Int = 0
  def freshName() = {
    tpvNum = tpvNum+1
    tpvNum.toString
  }
  def expandLB(lowerBound: UnifyLessDot, upperBound: UnifyLessDot, fc: FiniteClosure): Set[Set[UnifyConstraint]] ={
    def convert(fjType: FJType): UnifyType = fjType match {
      case FJNamedType(n, p) => UnifyRefType(n, p.map(convert))
      case FJTypeVariable(n) => UnifyTV("$"+n)
    }
    val b:UnifyTV = lowerBound.right.asInstanceOf[UnifyTV]
    val lowerBoundType : UnifyRefType = lowerBound.left.asInstanceOf[UnifyRefType]
    val upperBoundType : UnifyRefType = upperBound.right.asInstanceOf[UnifyRefType]
    fc.superTypes(lowerBoundType.name)//.filter(t => fc.aIsSubtypeOfb(t, convertRefType(upperBoundType)))
      .map(t => {
        def getUnifyTV(of: FJType): Set[UnifyTV] = of match{
          case FJTypeVariable(a) => Set(UnifyTV(a))
          case FJNamedType(_, params) => params.flatMap(getUnifyTV(_)).toSet
        }
        val superType = t._2
        val superTypeTVs = superType.params.flatMap(getUnifyTV(_))
        val freshWildcards = superTypeTVs.map(tv =>
          (tv, UnifyWildcard(freshName(), UnifyTV(freshName()), UnifyTV(freshName()))))
        val wildcardCons : Set[UnifyConstraint] = freshWildcards.map(wc => UnifyLessDot(wc._2.lowerBound, wc._2.upperBound)).toSet
        val cons : Set[UnifyConstraint] = Set(UnifyEqualsDot(b, convert(superType)), UnifyLessDot(lowerBoundType, convert(superType))) ++
          wildcardCons
        var ret = cons
        freshWildcards.foreach(wc => ret = subst(wc._1, wc._2, ret))
        ret
      })
  }

  private def getUpperBoundOrSetUpperBoundToObject(forTV: UnifyTV, eq: Set[UnifyConstraint]) = eq.find(_ match {
    case UnifyLessDot(UnifyTV(a), UnifyRefType(n, params)) => UnifyTV(a).eq(forTV)
    case _ => false
  }).getOrElse(UnifyLessDot(forTV, UnifyRefType("Object", List()))).asInstanceOf[UnifyLessDot]

  def step2(eq : Set[UnifyConstraint], fc: FiniteClosure) ={
    val cpBuilder = new CartesianProductBuilder[Set[UnifyConstraint]]()
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]

    eq.foreach(cons => cons match {
      case UnifyLessDot(UnifyRefType(n, ps), UnifyTV(a)) =>
        val lowerBound = UnifyLessDot(UnifyRefType(n, ps), UnifyTV(a))
        val upperBound = getUpperBoundOrSetUpperBoundToObject(lowerBound.right.asInstanceOf[UnifyTV], eq)
        cpBuilder.add(expandLB(lowerBound, upperBound, fc))
      case UnifyLessDot(UnifyTV(a), UnifyRefType(n, ps)) =>
        getLinks(UnifyTV(a), aUnifyLessDota)
          .map(b => {
            val upperBound = getUpperBoundOrSetUpperBoundToObject(b, eq)
            val lowerBound = UnifyLessDot(UnifyRefType(n, ps), b)
            //ExpandLB and add to return constraint set + {b <. C<T>} constraint
            cpBuilder.add(expandLB(lowerBound, upperBound, fc) + Set(UnifyLessDot(b, b)))
          })
        cpBuilder.addSingleton(Set(UnifyLessDot(UnifyTV(a), UnifyRefType(n, ps))))
        //the upper bound constraint remains in the constraint set:
        cpBuilder.addSingleton(Set(UnifyLessDot(UnifyTV(a), UnifyRefType(n, ps))))
      case cons => cpBuilder.addSingleton(Set(cons))
    })
    cpBuilder.build()
  }

  private def getAUnifyLessDotC(from: Set[UnifyConstraint]) = from.filter(c => c match{
    case UnifyLessDot(UnifyTV(_), UnifyRefType(_,_)) => true
    case _ => false
  }).asInstanceOf[Set[UnifyLessDot]]

  def matchRule(eq : Set[UnifyConstraint], fc: FiniteClosure) = {
    val aUnifyLessDotC = getAUnifyLessDotC(eq)
    (eq -- aUnifyLessDotC) ++ aUnifyLessDotC.flatMap(c => {
      val smallerC = aUnifyLessDotC.filter(c2 => c2 != c && c2.left.equals(c.left) && fc.isPossibleSupertype(c2.right.asInstanceOf[UnifyRefType].name,c.right.asInstanceOf[UnifyRefType].name))
      if(smallerC.isEmpty){
        List(c)
      }else{
        val list = smallerC.toList
        UnifyLessDot(list.head.right, c.right) :: list.tail
      }
    }
    )
  }

  def reduceEqRule(eq: Set[UnifyConstraint]) = eq.flatMap(c => c match {
    case UnifyEqualsDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)) => ap.zip(bp).map(p => UnifyEqualsDot(p._1, p._2))
    case x => Set(x)
  })

  def reduceRule(eq: Set[UnifyConstraint]) =  eq.flatMap(c => c match {
    case UnifyLessDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)) => {
      if(an.equals(bn)){
        val wildcardMap = ap.zip(bp).collect(p => p._2 match {
          case UnifyWildcard(n, u, l) => (UnifyWildcard(n, u, l) -> p._1)
        })
        val doteqs: Set[UnifyConstraint] = ap.zip(bp).flatMap(p => p._2 match {
          case UnifyWildcard(_,_,_) => None
          case x => Some(UnifyEqualsDot(p._1, x))
        }).toSet
        val wildcardCons: Set[UnifyConstraint] = ap.zip(bp).collect(p => p._2 match {
          case UnifyWildcard(n, u, l) =>
            Set(UnifyEqualsDot(p._1, p._2), UnifyLessDot(p._1, u), UnifyLessDot(l, p._1))
        }).flatten.toSet
        var ret = wildcardCons ++ doteqs
        wildcardMap.foreach(p => {
          ret = substWC(p._1, p._2, ret)
        })
        ret
      }else{
        Set(UnifyEqualsDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)))
      }
    }
    case x => Set(x)
  })

  def wildcardRules(eq: Set[UnifyConstraint]): Set[UnifyConstraint] = eq.flatMap(c => c match {
    case UnifyLessDot(UnifyWildcard(n, u, l), t) => Set(UnifyLessDot(u, t))
    case UnifyLessDot(t, UnifyWildcard(n, u, l)) => Set(UnifyLessDot(t, l))
    case UnifyEqualsDot(UnifyWildcard(n, u, l), t) => Set(UnifyEqualsDot(u, t), UnifyEqualsDot(l,t))
    case UnifyEqualsDot(UnifyWildcard(n, u, l), UnifyWildcard(n2, u2, l2)) => if (n.eq(n2)) {
      Set()
    } else {
      Set(UnifyEqualsDot(l, u), UnifyEqualsDot(u2, l2), UnifyEqualsDot(l, l2))
    }
    case x => Set(x)
  })

  def swapRule(eq : Set[UnifyConstraint]) = eq.map(c => c match {
    case UnifyEqualsDot(UnifyRefType(an, ap), UnifyTV(a)) => UnifyEqualsDot(UnifyTV(a), UnifyRefType(an, ap))
    case UnifyEqualsDot(UnifyWildcard(n,u,l), UnifyTV(a)) => UnifyEqualsDot(UnifyTV(a), UnifyWildcard(n,u,l))
    case x => x
  })

  private def getSuperTypes(of: UnifyRefType, fc: FiniteClosure) = fc.superTypes(of.name)

  def adaptRule(eq: Set[UnifyConstraint], fc: FiniteClosure) = {
    def paramSubst(param : FJType, paramMap : Map[FJType, UnifyType]): UnifyType = param match{
      case FJNamedType(n, params) => UnifyRefType(n, params.map(paramSubst(_, paramMap)))
      case typeVariable => paramMap.get(typeVariable).get
    }
    eq.map(c => c match {
      case UnifyLessDot(UnifyRefType(an, ap), UnifyRefType(bn, bp)) =>
        getSuperTypes(UnifyRefType(an, ap), fc).find(r => r._2.name.equals(bn)) match {
          case Some(subtypeRelation) =>
            val paramMap = subtypeRelation._1.params.zip(ap).toMap
            val newParams = subtypeRelation._2.params.map(paramSubst(_, paramMap))
            UnifyLessDot(UnifyRefType(subtypeRelation._2.name, newParams), UnifyRefType(bn, bp))
          case None => UnifyLessDot(UnifyRefType(an, ap), UnifyRefType(bn, bp))
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

  /**
   * Every 'b' with a <.* b
   */
  def getLinks(a: UnifyTV, aUnifyLessDota: Set[UnifyLessDot]): Set[UnifyTV] =
    aUnifyLessDota.filter(c => c.left.asInstanceOf[UnifyTV].name.equals(a.name))
      .flatMap(cons => Set(cons.right.asInstanceOf[UnifyTV]) ++ getLinks(cons.right.asInstanceOf[UnifyTV], aUnifyLessDota))


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

  def equalsRule(eq: Set[UnifyConstraint]) ={
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]
    def getRightSides(of: UnifyTV) ={
      aUnifyLessDota.filter(c => c.left.asInstanceOf[UnifyTV].name.equals(of.name))
    }
    def findCircle(start: UnifyTV) : List[UnifyLessDot] = findCircleRec(start, Set(start))
    def findCircleRec(next: UnifyTV, visited: Set[UnifyTV]): List[UnifyLessDot] = {
      val rightSides = getRightSides(next).iterator
      while(rightSides.hasNext){ //Deep search
        val rightSide = rightSides.next()
        val nextTV = rightSide.right.asInstanceOf[UnifyTV]
        if(visited.contains(nextTV)){
          return List(rightSide)
        }else{
          return rightSide :: findCircleRec(nextTV, visited + nextTV)
        }
      }
      List() // empty list means there are no circles
    }
    val circle = aUnifyLessDota.map(cons => findCircle(cons.left.asInstanceOf[UnifyTV])).find(!_.isEmpty)
    if(circle.isDefined){
      val newEq = eq -- circle.get
      Some(newEq ++ (circle.get.map(c => UnifyEqualsDot(c.left, c.right))))
    }else{
      None
    }
  }

  private def paramsContain(tv: UnifyTV, inType: UnifyType): Boolean =
    inType match {
      case UnifyTV(a) => tv.equals(UnifyTV(a))
      case UnifyRefType(a,p) => p.find(t => paramsContain(tv, t)).isDefined
      case UnifyWildcard(n, u, l) => paramsContain(tv, u) || paramsContain(tv, l)
    }
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

  private def substHelperWC(a: UnifyWildcard, withType: UnifyType,in: UnifyType) :UnifyType = in match {
    case UnifyRefType(n, p) => UnifyRefType(n,p.map(t => substHelperWC(a, withType, t)))
    case UnifyWildcard(n, u, l) =>
      if(a.name.equals(n)){withType}else{in}
  }

  def substWC(a: UnifyWildcard, substType: UnifyType,eq: Set[UnifyConstraint]): Set[UnifyConstraint] = {
    eq.map(c => c match {
      case UnifyLessDot(left, right) => UnifyLessDot(substHelperWC(a, substType, left), substHelperWC(a, substType, right))
      case UnifyEqualsDot(left, right) => UnifyEqualsDot(substHelperWC(a, substType, left), substHelperWC(a, substType, right))
    })
  }
  private def substHelper(a: UnifyTV, withType: UnifyType,in: UnifyType) :UnifyType = in match {
    case UnifyRefType(n, p) => UnifyRefType(n,p.map(t => substHelper(a, withType, t)))
    case UnifyTV(n) =>
      if(a.name.equals(n)){withType}else{in}
    case UnifyWildcard(name, upperBound, lowerBound) =>
      UnifyWildcard(name, substHelper(a, withType, upperBound), substHelper(a, withType, lowerBound))
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
      val adaptRuleResult = adaptRule(eqNew, fc)
      val adoptRuleResult = adoptRule(adaptRuleResult, fc)
      val matchRuleResult = matchRule(adoptRuleResult, fc)
      val reduceRuleResult = reduceRule(matchRuleResult)
      val reduceEqRuleResult = reduceEqRule(reduceRuleResult)
      val wildcardRulesResult = wildcardRules(reduceEqRuleResult)
      val swapRuleResult = swapRule(wildcardRulesResult)
      val eraseRuleResult = eraseRule(swapRuleResult)
      eqFinish = eraseRuleResult
    }while(!eqNew.equals(eqFinish))
    eqNew
  }

}

