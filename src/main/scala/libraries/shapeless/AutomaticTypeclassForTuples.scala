package libraries.shapeless

import scala.language.implicitConversions

import shapeless.ops.record.Keys
import shapeless.{::, Generic, HList, HNil, LabelledGeneric}

trait Bounded[A] {
  def top: A
  def bottom: A
  def cf(x: A, y: A): Int
}
object Bounded{
  implicit val dblb: Bounded[Double] = new Bounded[Double] {
    override def top = Double.PositiveInfinity
    override def bottom = Double.NegativeInfinity
    override def cf(x: Double, y: Double) = x.compareTo(y)
  }

  implicit val intb: Bounded[Int] = new Bounded[Int] {
    override def top = Int.MaxValue
    override def bottom = Int.MinValue
    override def cf(x: Int, y: Int) = x.compareTo(y)
  }

  implicit val boolb: Bounded[Boolean] = new Bounded[Boolean] {
    override def top = true
    override def bottom = false
    override def cf(x: Boolean, y: Boolean) = x.compareTo(y)
  }

  implicit val strb: Bounded[String] = new Bounded[String] {
    override def top = "z"*10
    override def bottom = ""
    override def cf(x: String, y: String) = x.compareTo(y)
  }

  implicit val hnilBounded: Bounded[HNil] = new Bounded[HNil] {
    override def top: HNil = HNil
    override def bottom: HNil = HNil
    override def cf(x: HNil, y: HNil): Int = 0
  }

  implicit def hlistBounded[H,T<:HList](implicit hb: Bounded[H],
                                        tb: Bounded[T]): Bounded[H :: T] = new Bounded[H :: T] {
    override def top: ::[H, T] = hb.top :: tb.top
    override def bottom: ::[H, T] = hb.bottom :: tb.bottom
    override def cf(x: ::[H, T], y: ::[H, T]): Int = if(hb.cf(x.head,y.head)!=0) hb.cf(x.head,y.head) else tb.cf(x.tail, y.tail)
  }

  implicit def hlistDefBounded[H,T<:HList](implicit defh: Defaultable[H],
                                           tb: Bounded[T]): Bounded[H :: T] = new Bounded[H :: T] {
    override def top: ::[H, T] = defh.default :: tb.top
    override def bottom: ::[H, T] = defh.default :: tb.bottom
    override def cf(x: ::[H, T], y: ::[H, T]): Int = tb.cf(x.tail, y.tail)
  }

  implicit def hlistordefBounded[H,T<:HList](implicit hb: Bounded[H],
                                             defh: Defaultable[H],
                                             tb: Bounded[T]): Bounded[H :: T] = new Bounded[H :: T] {
    override def top: ::[H, T] = defh.default :: tb.top
    override def bottom: ::[H, T] = defh.default :: tb.bottom
    override def cf(x: ::[H, T], y: ::[H, T]): Int = tb.cf(x.tail, y.tail)
  }

//  This Makes app code below fail statically
//  implicit def boundedOnlyHead[H,T<:HList](implicit hb: Bounded[H],
//                                           deft: Defaultable[T]): Bounded[H :: T] = new Bounded[H :: T] {
//    override def top: ::[H, T] = hb.top :: deft.default
//    override def bottom: ::[H, T] = hb.bottom :: deft.default
//    override def cf(x: ::[H, T], y: ::[H, T]): Int = hb.cf(x.head, y.head)
//  }

  implicit def genericBounded[A, R](implicit gen: Generic[A] { type Repr = R },
                                    reprb: Bounded[R]): Bounded[A] = new Bounded[A] {
    override def top = gen.from(reprb.top)
    override def bottom = gen.from(reprb.bottom)
    override def cf(x: A, y: A) = reprb.cf(gen.to(x), gen.to(y))
  }

  def min[A: Bounded](x: A, y: A): A = if(implicitly[Bounded[A]].cf(x,y) <=0 ) x else y
  def top[A: Bounded](x: A): A = implicitly[Bounded[A]].top
  def bottom[A: Bounded](x: A): A = implicitly[Bounded[A]].bottom
}

trait Defaultable[A]{ def default:A }
object Defaultable{
  def apply[A:Defaultable]: Defaultable[A] = implicitly

  implicit val defstr = new Defaultable[String] {
    override def default: String = "churro"
  }

  implicit val hnilBounded: Defaultable[HNil] = new Defaultable[HNil] {
    override def default: HNil = HNil
  }

  implicit def hlistBounded[H,T<:HList](implicit hd: Defaultable[H],
                                        td: Defaultable[T]): Defaultable[H :: T] = new Defaultable[H :: T] {
    override def default: H :: T = hd.default :: td.default
  }

  implicit def genericDefaultable[A, R](implicit gen: Generic[A] { type Repr = R },
                                        reprd: Defaultable[R]): Defaultable[A] = new Defaultable[A] {
    override def default: A = gen.from(reprd.default)
  }

  def default[A:Defaultable]: A = implicitly[Defaultable[A]].default
}

object AutomaticTypeclassForTuples extends App {

  val tpl1 = (77.0, "a", 5, false, 10)
  val tpl2 = (77.0, "a", 5, false, 9)

  {
    import Bounded.hlistBounded
    // needs to disambiguate when a T is both Bounded and Defaultable
    // with this, Bounded wins

    println(">>>>>>>>>>>>>>>>>>>>>\n>>> BOUNDED precedence")
    println(Bounded.min(tpl1, tpl2))
    println(Bounded.top(tpl1))
    println(Bounded.bottom(tpl1))
  }
  {
    import Bounded.hlistordefBounded
    // needs to disambiguate when a T is both Bounded and Defaultable
    // with this, Defaultable wins

    println("\n\n>>> DEFAULTABLE precedence")
    println(Bounded.min(tpl1, tpl2))
    println(Bounded.top(tpl1))
    println(Bounded.bottom(tpl1))
  }

  println("\n\n>>> WANT TO APPLY BOUNDED ONLY TO nth COMPONENT")
  val tpl3 = (77.0, 4.4)
  val tpl4 = (77.0, 8.8)
  println(Bounded.min(tpl3,tpl4))

  println("\n\n>>> LabelledGeneric: keys")
  val labl = LabelledGeneric[(Double, String, Int, Boolean, Int)]
  val keys = Keys[labl.Repr].apply
  println(keys)

  println("\n\n>>> Defaultable collection")
  println(Defaultable[(String,String,String)].default)
}
