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
    val cLessdotACons: Set[Set[Constraint]] = eq.map(c => c match{
      case LessDot(RefType(name,params), TypeVariable(a)) =>
        fc.superTypes(name)
        .map(superType => EqualsDot(TypeVariable(a), RefType(superType.name, superType.params.map(fc.getFCType(name).params.zip(params).toMap))).asInstanceOf[Constraint])
      case _ => null
    }).filter(s => s!=null)
    //val mutatedCLessdotACons = cartesianProduct(cLessdotACons)
    val aLessdotCCons: Set[Set[Constraint]] = eq.map(c => c match{
      case LessDot(TypeVariable(a),RefType(name,params)) =>
        fc.subTypes(name)
          .map(subTypes => EqualsDot(TypeVariable(a), RefType(subTypes.name, subTypes.params.map(fc.getFCType(name).params.zip(params).toMap))).asInstanceOf[Constraint])
      case _ => null
    }).filter(s => s!=null)
    //val mutadedALessdotCCons = cartesianProduct(aLessdotCCons)
    val eqSet = cartesianProduct((Set(Set(eq1))+cLessdotACons+aLessdotCCons).filter(s => !s.isEmpty))
    eqSet.map( s => s.flatten)
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

