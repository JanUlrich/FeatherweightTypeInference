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
    ast.flatMap(c =>{
      val genericBounds: Set[(UnifyRefType, UnifyRefType)] = c.genericParams.map(gt => (convertType(gt._1).asInstanceOf[UnifyRefType], convertType(gt._2).asInstanceOf[UnifyRefType])).toSet
      val classExtension: (UnifyRefType, UnifyRefType) = (cToUnifyType(c), convertType(c.superType).asInstanceOf[UnifyRefType])
      genericBounds + classExtension
    }
  ).toSet)
  private def cToUnifyType(c: Class): UnifyRefType = UnifyRefType(c.name, c.genericParams.map(it => convertType(it._1)))

  def typeinference(str: String): Either[String, Set[Set[UnifyConstraint]]] = {
    val ast = Parser.parse(str).map(ASTBuilder.fromParseTree(_))

    val typeResult = ast.map(ast => {
      var unifyResults = Set[Set[Set[UnifyConstraint]]]()
      ast.foldLeft(List[Class]())((cOld, c) => {
        val newClassList = cOld :+ c
        val typeResult = TYPE.generateConstraints(newClassList, generateFC(newClassList))
        val unifyResult = Unify.unify(convertOrConstraints(typeResult._1), typeResult._2)
        //Insert intersection types
        val typeInsertedC = InsertTypes.insert(unifyResult, c)
        unifyResults = unifyResults + unifyResult
        cOld :+ typeInsertedC
      })
      unifyResults
    })
    typeResult.map(_.flatten)
  }
}
