package jt.scalaplay

import scala.reflect.runtime.{universe => ru}
import java.util.concurrent.atomic.AtomicInteger

object Nodes {
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  case class Reference(table: String, key: Any)

  trait Node {
    private lazy val id = Node.nextIndex
    def key : Any = id

    def typeId : String // TODO From reflection  = getTypeTag(this).tpe.typeSymbol.toString
    def serialize : Map[String,Any] = Map("id" -> id) ++ serializeSelf

    def serializeSelf : Map[String,Any] 
    def reference : Reference  = Reference(typeId, key)
    def dependencies : Seq[Node] = Seq.empty
    def register : Set[Node] = Set(this) ++ dependencies.flatMap(_.register)
  }

  object Node {
    private val index = new AtomicInteger(-1)
    def nextIndex = index.incrementAndGet()
  }

  trait Src[T] extends Node {
    def value: T
  }

  class Gad extends Node {
    val bog = new DP("bog")
    val est = new DP("est")
    val root = new BogEst(bog, est)

    def serializeSelf = Map(
        "bog" -> bog.reference,
        "root" -> root.reference
    )
    def typeId = "Gad"
    override def dependencies = Seq(bog, est, root)
  }

  case class Calced[T](val value: T) extends Src[T] {
    def serializeSelf = Map("value" -> value)
    def typeId = "Calced"
  }

  class BogEst(bogSrc: Src[Int], estSrc: Src[Int]) extends Node {
    def bog = bogSrc.value
    def est = estSrc.value

    def diff = Calced(bog - est)

    def serializeSelf = Map(
        "bog" -> bogSrc.reference,
        "est" -> estSrc.reference
    )
    def typeId = "BogEst"
    override def dependencies = Seq(bogSrc, estSrc, diff)
  }

  class DP(name: String) extends Src[Int] {
    def value = 10
    def serializeSelf = Map(
        "name" -> name
    )
    def typeId = "DP"
  }

  val g = new Gad
  val rows = g.register
  val tbls = rows.groupBy(_.typeId)
  
  val ser = tbls.mapValues(_.map(_.serialize))
  def main(args: Array[String]) = {
    println(ser)
  }
}
