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
  def convertType(t: Type): UnifyType = t match {
    case GenericType(name) => UnifyRefType(name, List())
    case RefType(n, p) => UnifyRefType(n,p.map(convertType))
    case TypeVariable(n) => UnifyTV(n)
  }

  private def convertSingleConstraint(constraint: Constraint) = constraint match {
    case LessDot(l, r) => UnifyLessDot(convertType(l),convertType(r))
    case EqualsDot(l, r) => UnifyEqualsDot(convertType(l),convertType(r))
    case _ => throw new Exception("Error: Möglicherweise zu tiefe Verschachtelung von OrConstraints")
  }

  private def generateFC(ast: List[Class]): FiniteClosure = new FiniteClosure(
    ast.map(c => (cToUnifyType(c), convertType(c.superType).asInstanceOf[UnifyRefType])).toSet)
  private def cToUnifyType(c: Class) = UnifyRefType(c.name, c.params.map(it => convertType(it._1)))

  def typeinference(str: String): Either[String, Set[Set[UnifyConstraint]]] = {
    val ast = Parser.parse(str).map(ASTBuilder.fromParseTree(_))
    val typeResult = ast.map(ast => {
      /*ast.foldLeft(List())((cOld, c) => {
        val typeResult = TYPE.generateConstraints(ast, generateFC(ast))
        val unifyResult = Unify.unify(convertOrConstraints(typeResult._1), typeResult._2)
        //TODO: Insert intersection types
        List(c)
      }) */
      TYPE.generateConstraints(ast, generateFC(ast))
    })
    val unifyResult = typeResult.map(res => Unify.unify(convertOrConstraints(res._1), res._2))
    unifyResult
  }
}
