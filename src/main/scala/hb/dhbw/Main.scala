package hb.dhbw

import scala.util.Try
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal}
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.{Event, HTMLTextAreaElement, TextEvent, UIEvent}

import scala.scalajs.js

@js.native
@JSGlobal
object hljs extends js.Object {
  def highlightAuto(code: String): HLJSResult = js.native
}

// @ScalaJSDefined
class HLJSResult extends js.Object{
  val value: String = ""
}

object Main {

  def main(args: Array[String]): Unit = {
    val source = document.querySelector("#fj-input")
    update(source.textContent)
    source.addEventListener("input", typecheck)
  }
  @JSExportTopLevel("typecheck")
  def typecheck(e: UIEvent): Unit = {
    e.target match {
      case elt: HTMLTextAreaElement =>
        update(elt.value)
    }
  }

  def update(str: String): Unit = {
    val target = document.querySelector("#unify-output")
    val tiResult = FJTypeinference.typeinference(str)
    target.innerHTML = tiResult.fold(
      (error) => error,
      (result) => prettyPrintHTML(result._1)
    )
    val astOutput = document.querySelector("#ast-output")
    astOutput.innerHTML = tiResult.fold(
      (error) => Parser.parse(str).map( parseTree =>
        hljs.highlightAuto(prettyPrintAST(ASTBuilder.fromParseTree(parseTree))).value
      ).merge,
      (result) => {
          hljs.highlightAuto(prettyPrintAST(result._2)).value
      }
    )

  }

  def prettyPrintAST(ast: List[Class]): String = {
    def prettyPrintExpr(expr: Expr): String = expr match {
      case LocalVar(x) => x
      case FieldVar(e, f) => prettyPrintExpr(e)+"."+f
      case MethodCall(e, name, params) => prettyPrintExpr(e)+"."+name+"("+params.map(prettyPrintExpr(_)).mkString(", ")+")"
      case Constructor(className, params) => "new "+className+"(" + params.map(prettyPrintExpr(_)).mkString(", ") +")"
    }
    def prettyPrintType(l: Type): String = l match {
      case RefType(name, List()) => name
      case RefType(name, params) => name + "<" + params.map(prettyPrintType(_)).mkString(", ") + ">"
      case GenericType(name) => name
    }
    def prettyPrintCons(constraint: Constraint)= constraint match{
      case LessDot(l, r) => prettyPrintType(l) + " extends " + prettyPrintType(r)
    }
    def prettyPrintGenericList(constraints: List[Constraint]) = {
      if(constraints.isEmpty){
        ""
      }else{
        "<" + constraints.map(prettyPrintCons(_)).mkString(", ") + ">"
      }
    }
    ast.map(cl => {
      "class " + cl.name + prettyPrintGenericList(cl.genericParams.map(it => LessDot(it._1, it._2))) +
        " extends " + prettyPrintType(cl.superType) + "{\n" +
        cl.fields.map(f => {
          "    " + prettyPrintType(f._1) + " " + f._2 + ";"
        }).mkString("\n") + "\n" +
        cl.methods.map(m => {
          "    "+ prettyPrintGenericList(m.genericParams) + " " +
           prettyPrintType(m.retType) +" "+ m.name +"(" + m.params.map(tp=>prettyPrintType(tp._1) + " " + tp._2).mkString(", ") + ") {\n"+
          "        return " + prettyPrintExpr(m.retExpr) + ";\n" +
          "    }"
      }).mkString("\n") + "\n}"
    }).mkString("\n")
  }

  def prettyPrintHTML(c: Class): String = {
    "class "+c.name+" extends " + prettyPrintHTML(c.superType) + "{</br>" +
      "<p class=\"code-line\">"+
    c.fields.map(f => prettyPrintHTML(f._1)+" "+f._2+";").mkString("</br>")+"</br>" +
      c.methods.map(prettyPrintHTML(_)).mkString("</br>")+"</br>}" +
      "</p>"
  }
  def prettyPrintHTMLAST(ast: List[Class]): String = {
    ast.map(prettyPrintHTML(_)).mkString("</br>")
  }
  def prettyPrintHTML(method: Method): String = {
    prettyPrintHTML(method.retType) + " " +
     method.name + "(" + method.params.map(p => prettyPrintHTML(p._1) + " " + p._2).mkString(", ") + ") { </br>" +
      "<p class=\"code-line\">"+
      "return "+prettyPrintHTML(method.retExpr)+";" +
    "</p></br>}"
  }
  def prettyPrintHTML(expr : Expr): String = expr match {
    case MethodCall(e, name, params) => prettyPrintHTML(e)+"."+name+"("+params.map(prettyPrintHTML(_)).mkString(", ")+")"
    case FieldVar(e, f) => prettyPrintHTML(e)+"."+f
    case LocalVar(x) => x
    case Constructor(className, params) => "new "+className+"("+params.map(prettyPrintHTML(_)).mkString(", ")+")"
  }

  def prettyPrintHTML(unifyResult : Set[Set[UnifyConstraint]]): String = unifyResult.map(
    solution => "<p>{" + solution.map(prettyPrintHTML(_)).mkString(", ") + "}</p>"
  ).foldLeft("Solutions:")(_ + "" +  _)
  def prettyPrintHTML(cons: UnifyConstraint): String = cons match {
    case UnifyLessDot(a, b) => "("+prettyPrintHTML(a)+" <. "+prettyPrintHTML(b)+")"
    case UnifyEqualsDot(a, b) => "("+prettyPrintHTML(a)+" =. "+prettyPrintHTML(b)+")"
  }
  def prettyPrintHTML(t: UnifyType): String = t match {
    case UnifyRefType(name, List()) => name
    case UnifyRefType(name, params) => name + "&lt;" + params.map(prettyPrintHTML).mkString(", ") + "&gt;"
    case UnifyTV(name) => "<b>" + name + "</b>"
  }
  def prettyPrintHTML(t: Type): String = t match {
    case RefType(name, List()) => name
    case RefType(name, params) => name + "&lt;" + params.map(prettyPrintHTML).mkString(", ") + "&gt;"
    case TypeVariable(name) => "<b>" + name + "</b>"
    case GenericType(name) => name
  }

  def prettyPrint(unifyResult : Set[Set[UnifyConstraint]]): String = unifyResult.map(
    solution => "{" + solution.map(prettyPrint(_)).mkString(", ") + "}"
  ).foldLeft("Solutions:")(_ + "\n\n" +  _)
  def prettyPrint(cons: UnifyConstraint): String = cons match {
    case UnifyLessDot(a, b) => "("+prettyPrint(a)+" <. "+prettyPrint(b)+")"
    case UnifyEqualsDot(a, b) => "("+prettyPrint(a)+" =. "+prettyPrint(b)+")"
  }
  def prettyPrint(t: Type): String = t match {
    case RefType(name, List()) => name
    case RefType(name, params) => name + "<" + params.map(prettyPrint).mkString(", ") + ">"
    case TypeVariable(name) => "_" + name + "_"
    case GenericType(name) => name
  }
  def prettyPrint(t: UnifyType): String = t match {
    case UnifyRefType(name, List()) => name
    case UnifyRefType(name, params) => name + "<" + params.map(prettyPrint).mkString(", ") + ">"
    case UnifyTV(name) => "_" + name + "_"
  }
}
