package jt.scalaplay

object Scratch {
  class A {
    val x = (t:Int) => t*2
  }
  class A2(val xs: Seq[Int], orig: A) extends A {
    override val x = xs.map(t => t -> orig.x(t)).toMap
  }

  def mapify[T,R](ts: Seq[T], f: T => R) : Map[T,R] = ts.map(t => t -> f(t)).toMap

  trait B {
    def x(t: Int) : Int

    case class Affixed(x: Map[Int, Int])
    def affixed(xs: Seq[Int]) = Affixed(mapify(xs, x))
  }

  trait C {
    def b1: B
    def b2: B

    case class Affixed(b1: B#Affixed, b2: B#Affixed)
    def affixed(xs: Seq[Int]) = Affixed(b1.affixed(xs), b2.affixed(xs))
  }

  class BImpl extends B {
    def x(t: Int) = t*2
  }

  class CImpl extends C {
    val b1 = new BImpl()
    val b2 = new BImpl()
  }

  def main(args: Array[String]) {
    val a = new A()
    println(a.x(4))
    val a2 = new A2(Seq(1,2,3),a)
    println(a2.x(1))
    val c = new CImpl()
    println(c.affixed(Seq(1,2,3)))
  }
}
