package hb.dhbw

sealed trait FJType
case class FJNamedType(name: String, params: List[FJType]) extends FJType
case class FJTypeVariable(name: String) extends FJType


class FiniteClosure(val extendsRelations : Set[(FJNamedType, FJNamedType)]){

  private def calculateSupertypes(of: FJNamedType) ={
    var rel = Set((of, of))
    var size = rel.size
    do {
      size = rel.size
      rel = rel ++ reflexiveTypes(rel) ++ transitiveTypes(rel) ++ superClassTypes(rel)
    }while(rel.size > size)
    rel.map(_._2)
  }
  private def reflexiveTypes(of: Set[(FJNamedType, FJNamedType)]) ={
    val ref = Set.newBuilder[(FJNamedType, FJNamedType)]
    ref ++= of.map(pair => (pair._1, pair._1))
    ref ++= of.map(pair => (pair._2, pair._2))
    ref.result()
  }
  private def transitiveTypes(of: Set[(FJNamedType, FJNamedType)]) ={
    val ref = Set.newBuilder[(FJNamedType, FJNamedType)]
    ref ++= of.flatMap(pair => of.filter(p =>  p._1.eq(pair._2)))
    ref.result()
  }
  private def superClassTypes(of: FJNamedType) = {
    val extendsRelation = extendsRelations.filter(pair => pair._1.name.equals(of.name))
    extendsRelation.map(p => {
      val paramMap = p._1.params.zip(of.params).toMap
      (of,FJNamedType(p._2.name, p._2.params.map(paramMap)))
    })
  }
  private def superClassTypes(of: Set[(FJNamedType, FJNamedType)]) : Set[(FJNamedType, FJNamedType)] ={
    val sClass = Set.newBuilder[(FJNamedType, FJNamedType)]
    sClass ++= of.flatMap(pair => Set(pair._2, pair._1)).flatMap(t => superClassTypes(t))
    sClass.result()
  }

  def superTypes(of : FJNamedType) : Set[FJNamedType] = calculateSupertypes(of)

  def aIsSubtypeOfb(a: FJNamedType, b: FJNamedType): Boolean = calculateSupertypes(a).contains(b)

  def isPossibleSupertype(of: String, superType: String): Boolean = {
    val extendsMap = extendsRelations.map(p => (p._1.name,p._2.name)).toMap
    var subType = of
    var isSuperType = false
    if(subType.equals(superType)) isSuperType = true
    while(extendsMap.contains(subType)){
      subType = extendsMap.get(subType).get
      if(subType.equals(superType)) isSuperType = true
    }
    isSuperType
  }
}