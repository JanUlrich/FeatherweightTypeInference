package hb.dhbw


sealed abstract class Type
final case class TypeVariable(name : String) extends Type
final case class RefType(name : String, params : List[Type]) extends Type

class FiniteClosure(val extendsRelations : Set[(RefType, RefType)]){

  def superTypes(of : String) : Set[RefType] = Set(extendsRelations.map(p => (p._1.name, p._2)).toMap.get(of).get)
  // extendsRelations.filter(p => p._1.name == of).map(p => superTypes(p._1.name)).flatten + extendsRelations.filter(p => p._1.name == of).map(_._1).head
  def subTypes(of : String) : Set[RefType] =Set(extendsRelations.map(p => (p._1.name, p._2)).toMap.get(of).get)
    //extendsRelations.filter(p => p._2.name == of).map(p => subTypes(p._2.name)).flatten + extendsRelations.filter(p => p._1.name == of).map(_._1).head

    def getFCType(name : String): RefType ={
      extendsRelations.map(it => it._1).find(it => it.name == name).get
    }
  /*

  def cartesianProduct[A](lists : List[Set[A]]) : Set[List[A]] ={

    def listMultiply[A](inFront: Set[A], list: Set[List[A]]) = list.flatMap(s => inFront.map(element => element :: s))

    if(lists.size == 1) {
      lists.head.map(it => List(it))
    }
    else{
      listMultiply(lists.head, cartesianProduct(lists.tail))
    }

  }

    //Kann wiederverwendet werden:
    def replaceParams(newType : RefType, replace : RefType): RefType ={
      val fcType = getFCType(replace.name)
      val replaceMap : Map[TypeVariable, Type] = fcType.params.map{case t: TypePlaceholder => t}.zip(replace.params).toMap
      RefType(newType.name, newType.params.map{case t: TypePlaceholder => replaceMap(t) case r: RefType => replaceParams(r, replace) })
    }

    //TODO: Falsche Funktionen
    def greater(than : SimpleType): Set[SimpleType] = {
      than match {
        case RefType(name, params) => {
          val superTypesNoWildcard = superTypes(name).map(superType => replaceParams(superType, RefType(name, params) ))
          superTypesNoWildcard
            .flatMap(superType => {
              cartesianProduct(superType.params.map(param => greaterArg(param)))
                .map(parameterList => RefType(superType.name, parameterList))
            }
            )
        }
        case TypePlaceholder(name) => Set(than)
      }
    }

    def smaller(than : SimpleType): Set[SimpleType] = {
      than match {
        case RefType(name, params) => {
          val subTypesNoWildcard = subTypes(name).map(subType => replaceParams(subType, RefType(name, params) ))
          subTypesNoWildcard
            .flatMap(subTypes => {
              cartesianProduct(subTypes.params.map(param => smallerArg(param)))
                .map(parameterList => RefType(subTypes.name, parameterList))
            }
            )
        }
        case TypePlaceholder(name) => Set(than)
      }
    }

    def greaterArg(t : UnifyType): Set[UnifyType] ={
      t match { //TODO not recursive yet
        case RefType(name, params) => Set(t, SuperWildcard(RefType(name, params)), ExtendsWildcard(RefType(name, params)))
        case other => Set(other)
      }
    }

    def smallerArg(t : UnifyType): Set[UnifyType] ={
      t match { //TODO not recursive yet
        case RefType(name, params) => Set(t, SuperWildcard(RefType(name, params)), ExtendsWildcard(RefType(name, params)))
        case other => Set(other)
      }
    }
  */
}