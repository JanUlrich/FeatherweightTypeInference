package hb.dhbw


object Unify {

  sealed abstract class Constraint
  final case class LessDot(left: Type, right: Type) extends Constraint
  final case class EqualsDot(left: Type, right: Type) extends Constraint

  def unify(finiteClosure: FiniteClosure): Unit ={

  }

  def step1(eq : Set[Constraint], fc: FiniteClosure) ={
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
    //val mutatedCLessdotACons = cartesianProduct(cLessdotACons)
    val aLessdotCCons: Set[Set[Set[Constraint]]] = eq.map(c => c match{
      case LessDot(TypeVariable(a),RefType(name,params)) =>{
        val bs = eq1.filter(c => c match{
          case LessDot(TypeVariable(leftSide), TypeVariable(_)) => leftSide.equals(a)
          case _ => false
        }).map(p => p.asInstanceOf[LessDot].right).asInstanceOf[Set[TypeVariable]]
        bs.map(b => {
          val allSuperTypes = fc.superTypes(RefType(name,params))
            .map(superType => Set(EqualsDot(b, superType).asInstanceOf[Constraint]))
          Set(Set(LessDot(b, RefType(name,params))) ++ allSuperTypes
        )
        })
        fc.superTypes(RefType(name,params))
          .map(superType => Set(EqualsDot(TypeVariable(a), superType).asInstanceOf[Constraint]))
      }
      case _ => null
    }).filter(s => s!=null)
    //val mutadedALessdotCCons = cartesianProduct(aLessdotCCons)
    val eqSet = cartesianProduct(Set(Set(eq1)) ++ aLessdotCCons ++ cLessdotACons)
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
      newEq ++ (circle.get.map(c => EqualsDot(c.left, c.right)))
    }else{
      eq
    }
  }

  def applyRules(fc: FiniteClosure) ={
  }

  def cartesianProduct[A](lists : Set[Set[A]]) : Set[Set[A]] ={
    def listMultiply[A](inFront: Set[A], list: Set[Set[A]]) = list.flatMap(s => inFront.map(element => s + element))

    if(lists.size == 1) {
      lists.head.map(it => Set(it))
    }
    else{
      listMultiply(lists.head, cartesianProduct(lists.tail))
    }

  }
}

