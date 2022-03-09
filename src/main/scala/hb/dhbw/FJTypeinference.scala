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
  private def convertType(t: Type): UnifyType = t match {
    case GenericType(name) => UnifyRefType(name, List())
    case RefType(n, p) => UnifyRefType(n,p.map(convertType))
    case TypeVariable(n) => UnifyTV(n)
  }

  private def convertRefType(unifyType: UnifyRefType): FJNamedType = FJNamedType(unifyType.name, unifyType.params.map(convert(_)))

  private def convert(unifyType: UnifyType): FJType = unifyType match {
    case UnifyRefType(n, p) => FJNamedType(n, p.map(convert(_)))
    case UnifyTV(n) => FJTypeVariable(n)
  }

  private def convertSingleConstraint(constraint: Constraint) = constraint match {
    case LessDot(l, r) => UnifyLessDot(convertType(l),convertType(r))
    case EqualsDot(l, r) => UnifyEqualsDot(convertType(l),convertType(r))
    case _ => throw new Exception("Error: Internal Error considering Or-Constraints")
  }

  private def generateFC(ast: List[Class]): FiniteClosure = new FiniteClosure(
    ast.flatMap(c =>{
      val genericBounds: Set[(UnifyRefType, UnifyRefType)] = c.genericParams.map(gt => (convertType(gt._1).asInstanceOf[UnifyRefType], convertType(gt._2).asInstanceOf[UnifyRefType])).toSet
      val classExtension: (UnifyRefType, UnifyRefType) = (cToUnifyType(c), convertType(c.superType).asInstanceOf[UnifyRefType])
      genericBounds + classExtension
    }
  ).map(it => (convertRefType(it._1), convertRefType(it._2))).toSet)
  private def cToUnifyType(c: Class): UnifyRefType = UnifyRefType(c.name, c.genericParams.map(it => convertType(it._1)))

  private def removeOverloadedSubtypeMethods(in: Class, finiteClosure: FiniteClosure) = {
    def convertToFJType(in: Type): FJNamedType = in match {
      case GenericType(name) => FJNamedType(name, List())
      case RefType(n, p) => FJNamedType(n,p.map(convertToFJType))
    }
    def methodIsSupertype(m : Method, superMethod: Method) = {
      def getBound(t: Type): Type = t match {
        case GenericType(x) =>
          (in.genericParams ++ m.genericParams.map(c => (c.asInstanceOf[LessDot].l, c.asInstanceOf[LessDot].r)))
          .find(p => p._1.equals(GenericType(x))).map(_._2).map(getBound).get
        case x => x
      }
      if(m.params.size != superMethod.params.size){
        false
      }else{
        val returnIsSub = finiteClosure.aIsSubtypeOfb(convertToFJType(getBound(m.retType)), convertToFJType(getBound(superMethod.retType)))
        val paramsAreSup = m.params.zip(superMethod.params).foldLeft(true)((isSub, m2) => {
          isSub && finiteClosure.aIsSubtypeOfb(convertToFJType(getBound(m2._2._1)), convertToFJType(getBound(m2._1._1)))
        })
        returnIsSub && paramsAreSup
      }
    }

    val methodNames = in.methods.map(_.name).toSet
    val newMethods = methodNames.flatMap(mName => {
      val overloadedMethods = in.methods.filter(_.name.equals(mName))
      overloadedMethods.foldLeft(Set[Method]())((ms, m)=>{
        if(ms.find(methodIsSupertype(_, m)).isDefined) { //If there is a method which is more general
          ms //do not add this method
        }else { //otherwise check if this method shadows another method
          ms.filter(!methodIsSupertype(m, _)) + m
        }
      })
    })
    Class(in.name, in.genericParams, in.superType, in.fields, newMethods.toList)
  }

  def typeinference(str: String): Either[String, (Set[Set[UnifyConstraint]], List[Class])] = {
    val ast = Parser.parse(str).map(ASTBuilder.fromParseTree(_))
    var typedClasses: List[Class] = List()
    val typeResult = ast.map(ast => {
      var unifyResults = Set[Set[Set[UnifyConstraint]]]()
      ast.foldLeft(List[Class]())((cOld, c) => {
        val newClassList = cOld :+ c
        val typeResult = TYPE.generateConstraints(newClassList, generateFC(newClassList))
        val unifyResult = Unify.unifyIterative(convertOrConstraints(typeResult._1), typeResult._2)
        //Insert intersection types
        val typeInsertedC = InsertTypes.insert(unifyResult, c)
        typedClasses = typedClasses :+ typeInsertedC
        unifyResults = unifyResults + unifyResult
        cOld :+ typeInsertedC
      })
      unifyResults
    })
    val fc = generateFC(typedClasses)
    typedClasses =
    typedClasses.map(cl => {
      removeOverloadedSubtypeMethods(cl, fc)
    })
    typeResult.map(it => (it.flatten, typedClasses))
  }
}
