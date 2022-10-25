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

  /*
  Die generics sind für alle Methoden die gleichen. Falls dies der Fall ist, kann man einfach nach Sub typen in den Parametern und return-Typ filtern
   */
  private def removeOverloadedSubtypeMethods(in: Class, finiteClosure: FiniteClosure) = {
    def convertToFJType(in: Type): FJNamedType = in match {
      case GenericType(name) => FJNamedType(name, List())
      case RefType(n, p) => FJNamedType(n,p.map(convertToFJType))
    }
    def methodIsSupertype(m : Method, superMethod: Method) = false //TODO
    /*{
      if(m.genericParams.equals(superMethod.genericParams)) {
        val returnIsSub = finiteClosure.aIsSubtypeOfb(convertToFJType(m.retType), convertToFJType(superMethod.retType))
        val paramsAreSup = m.params.zip(superMethod.params).foldLeft(true)((isSub, m2) => {
          isSub && finiteClosure.aIsSubtypeOfb(convertToFJType(m2._2._1), convertToFJType(m2._1._1))
        })
        returnIsSub && paramsAreSup
      }else{
        false
      }
    }
    */
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

  def removeLessDotGenericConstraints(unifyResult: Set[Set[UnifyConstraint]], generics: Set[String]) :Set[Set[UnifyConstraint]] =
    unifyResult.map(_.map( _ match { //
      case UnifyLessDot(UnifyTV(a), UnifyRefType(name, List())) =>
        if(generics.contains(name))
            UnifyEqualsDot(UnifyTV(a), UnifyRefType(name, List()))
          else
            UnifyLessDot(UnifyTV(a), UnifyRefType(name, List()))
      case x => x
    }
  ))

  def typeinference(str: String): Either[String, List[Class]] = {
    val ast = Parser.parse(str).map(ASTBuilder.fromParseTree(_))
    var typedClasses: List[Class] = List()
    ast.map(ast => {
      ast.foldLeft(List[Class]())((cOld, c) => {
        val newClassList = cOld :+ c
        val fc = generateFC(newClassList)
        val typeResult = TYPE.generateConstraints(newClassList, fc)
        val unifyResult = Unify.unifyIterative(convertOrConstraints(typeResult._1), typeResult._2)

        val postProcessed = removeLessDotGenericConstraints(unifyResult, c.genericParams.map(_._1.asInstanceOf[GenericType].name).toSet)
        //Insert intersection types
        val typeInsertedC = removeOverloadedSubtypeMethods(InsertTypes.applyUnifyResult(postProcessed, c), fc)
        typedClasses = typedClasses :+ typeInsertedC
        cOld :+ typeInsertedC
      })
    })
  }
}
