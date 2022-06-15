package hb.dhbw

import hb.dhbw.Unify.getLinks


sealed abstract class UnifyConstraint(val left: UnifyType, val right: UnifyType)
final case class UnifyLessDot(override val left: UnifyType, override val right: UnifyType) extends UnifyConstraint(left, right)
final case class UnifyEqualsDot(override val left: UnifyType, override val right: UnifyType) extends UnifyConstraint(left, right)

sealed abstract class UnifyType
final case class UnifyRefType(name: String, params: List[UnifyType]) extends UnifyType
final case class UnifyTV(name: String) extends UnifyType

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

  def expandLB(lowerBound: UnifyLessDot, upperBound: UnifyLessDot, fc: FiniteClosure): Set[Set[UnifyConstraint]] ={
    val b:UnifyTV = lowerBound.right.asInstanceOf[UnifyTV]
    val lowerBoundType : UnifyRefType = lowerBound.left.asInstanceOf[UnifyRefType]
    val upperBoundType : UnifyRefType = upperBound.right.asInstanceOf[UnifyRefType]
    Set(fc.superTypes(convertRefType(lowerBoundType)).filter(t => fc.aIsSubtypeOfb(t, convertRefType(upperBoundType)))
      .map(t => UnifyEqualsDot(b, convertNamedType(t))))
  }

  private def getUpperBoundOrSetUpperBoundToObject(forTV: UnifyTV, eq: Set[UnifyConstraint]) = eq.find(_ match {
    case UnifyLessDot(UnifyTV(a), UnifyRefType(n, params)) => UnifyTV(a).eq(forTV)
    case _ => false
  }).getOrElse(UnifyLessDot(forTV, UnifyRefType("Object", List()))).asInstanceOf[UnifyLessDot]

  def step2(eq : Set[UnifyConstraint], fc: FiniteClosure) ={
    /*

    //Part one: use expandLB on lower Bound constraints:
    val lowerBoundConstraints : Set[UnifyLessDot] = eq.filter(_ match {
      case UnifyLessDot(UnifyRefType(_,_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]
    val lowerAndUpperBoundConstraints = lowerBoundConstraints.map(lowerBound => {
      val upperBound = getUpperBoundOrSetUpperBoundToObject(lowerBound.right.asInstanceOf[UnifyTV], eq)
      (lowerBound, upperBound)
    })
    //Add part one:
    lowerAndUpperBoundConstraints.map(bounds => cpBuilder.add(expandLB(bounds._1, bounds._2, fc)))

    // Part two: a <. Type, a <.* b constraints:
    //TODO
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]
    eq.filter(_ match {
      case UnifyLessDot(UnifyTV(_), UnifyRefType(_, _)) => true
      case _ => false
    }).map(cons => {
      val tv : UnifyTV = cons.left.asInstanceOf[UnifyTV]
      getLinks(tv, aUnifyLessDota)
        .map(b => {
          val upperBound = getUpperBoundOrSetUpperBoundToObject(b, eq)
          val lowerBound = UnifyLessDot(cons.right, b)
          //ExpandLB and add to return constraint set
          cpBuilder.add(expandLB(lowerBound, upperBound, fc) + Set(UnifyLessDot(b,b)))

        })
    })
     */
    val cpBuilder = new CartesianProductBuilder[Set[UnifyConstraint]]()
    val aUnifyLessDota = eq.filter(c => c match{
      case UnifyLessDot(UnifyTV(_), UnifyTV(_)) => true
      case _ => false
    }).asInstanceOf[Set[UnifyLessDot]]

    eq.foreach(cons => cons match {
      case UnifyLessDot(UnifyRefType(n, ps), UnifyTV(a)) => {
        val lowerBound = UnifyLessDot(UnifyRefType(n, ps), UnifyTV(a))
        val upperBound = getUpperBoundOrSetUpperBoundToObject(lowerBound.right.asInstanceOf[UnifyTV], eq)
        cpBuilder.add(expandLB(lowerBound, upperBound, fc))
      }
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

  /**
   * Every 'b' with a <.* b
   */
  private def getLinks(a: UnifyTV, aUnifyLessDota: Set[UnifyLessDot]): Set[UnifyTV] =
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

