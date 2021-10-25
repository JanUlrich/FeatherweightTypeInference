package hb.dhbw

object FJTypeinference {
  private def convertAndCons(andCons: Constraint): Set[UnifyConstraint] = andCons match {
    case AndConstraint(andCons) => andCons.map(convertSingleConstraint).toSet
    case c => Set(convertSingleConstraint(c))
  }
  private def convertOrCons(constraint: Constraint): Set[Set[UnifyConstraint]] = constraint match {
    case AndConstraint(andCons) => Set(andCons.map(convertSingleConstraint).toSet)
    case OrConstraint(orCons) => orCons.map(convertAndCons).toSet
    case x => Set(Set(convertSingleConstraint(x)))
  }
  private def convertOrConstraints(constraints: List[Constraint]): Set[Set[Set[UnifyConstraint]]] = constraints.map(
    convertOrCons
  ).toSet
  private def convertSingleConstraint(constraint: Constraint) = constraint match {
    case LessDot(l, r) => UnifyLessDot(l,r)
    case EqualsDot(l, r) => UnifyEqualsDot(l,r)
    case _ => throw new Exception("Error: Möglicherweise zu tiefe Verschachtelung von OrConstraints")
  }

  def typeinference(str: String): Either[String, Set[Set[UnifyConstraint]]] = {
    val ast = Parser.parse(str).map(ASTBuilder.fromParseTree(_))
    val typeResult = ast.map(TYPE.generateConstraints(_))
    val unifyResult = typeResult.map(res => Unify.unify(convertOrConstraints(res._1), res._2))
    unifyResult
  }
}
