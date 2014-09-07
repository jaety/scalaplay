package jt.scalaplay

import scala.reflect.runtime.{universe => ru}

object Nodes {
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  trait Node {
    def serialize(resolver: Node => Int) : Map[String,Any]
    // def reference(resolver: Node => Int) : (String, Int)
    def typeId : String // = getTypeTag(this).tpe.typeSymbol.toString
    def reference(resolver: Node => Int) : (String,Int) = (typeId, resolver(this))
  }

  trait Src[T] extends Node {
    def value: T
  }

//  trait Src[T] extends Node {
//    def src: Seq[Node] = Seq(this)
//    def value: T
//    def flatMap[B](f: T => Src[B]) : Src[B] = {
//        val derived = f(this.value)
//        val newSrc = src ++ derived.src
//        new Src[B] {
//            def src = newSrc
//            def value = derived.value
//        }
//    }
//  }

  class Gad extends Node {
    val bog = new DP("bog")
    val est = new DP("est")
    val root = new BogEst(bog, est)
    def serialize(resolver: Node => Int) = Map(
        "id" -> resolver(this),
        "bog" -> bog.reference(resolver),
        "root" -> root.reference(resolver)
    )
    def typeId = "Gad"
  }

  case class Calced[T](val value: T) extends Src[T] {
    def serialize(resolver: Node => Int) = Map("value" -> value)
    def typeId = "Calced"
  }

  class BogEst(bogSrc: Src[Int], estSrc: Src[Int]) extends Node {
    def bog = bogSrc.value
    def est = estSrc.value

    def diff = Calced(bog - est)

    def serialize(resolver: Node => Int) = Map(
        "id" -> resolver(this),
        "bog" -> bog.reference(resolver)
    )
    def typeId = "BogEst"
  }

  class DP(name: String) extends Src[Int] {
    def value = 10
    def serialize(resolver: Node => Int) = Map(
        "id" -> resolver(this),
        "name" -> name
    )
    def typeId = "DP"
  }

  val g = new Gad
  val rows = Seq(g, g.bog, g.root)
  val tbls = rows.groupBy(_.typeId)
  val indices = tbls.mapValues(_.zipWithIndex).values.flatten.toMap
  
  val ser = tbls.mapValues(_.map(_.serialize(indices)))
  def main(args: Array[String]) = {
    
  }
}
