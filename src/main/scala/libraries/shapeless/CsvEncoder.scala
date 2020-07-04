package libraries.shapeless

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait CsvEncoder[A] {
  def encode(a: A): List[String]
}

object CsvEncoder {
  // Summon
  def apply[A](implicit encA: CsvEncoder[A]): CsvEncoder[A] = encA

  // Constructor
  def instance[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    def encode(a: A): List[String] = func(a)
  }

  // Globally visible type class instances
  implicit val boolEncoder: CsvEncoder[Boolean] = new CsvEncoder[Boolean] {
    override def encode(a: Boolean): List[String] = List(if(a) "YES" else "NO")
  }
  implicit val stringEncoder: CsvEncoder[String] = instance(List(_))
  implicit val doubleEncoder: CsvEncoder[Double] = instance(d => List(d.toString))
  implicit val intEncoder: CsvEncoder[Int] = instance(d => List(d.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = instance(_ => Nil)
  implicit def hlistEncoder[H, T<:HList](implicit hEnc:Lazy[CsvEncoder[H]], tEnc:CsvEncoder[T]): CsvEncoder[H::T] =
    instance { case h :: t => hEnc.value.encode(h) ++ tEnc.encode(t) }

  implicit val cnilEncoder: CsvEncoder[CNil] = instance(_ => throw new Exception("CNIL"))
  implicit def coproductsEncoder[H,  T <: Coproduct](implicit hEnc: Lazy[CsvEncoder[H]], tEnc:CsvEncoder[T]): CsvEncoder[H:+:T] =
    instance { case Inl(h) => hEnc.value.encode(h); case Inr(t) => tEnc.encode(t) }

  //implicit def lstEncoder[T](lst: List[T])(implicit encT: CsvEncoder[T]): List[String] = lst.map(implicitly[CsvEncoder[T]].encode(_)).flatten

  implicit def genericEncoder[A,R](implicit genA: Generic[A] { type Repr = R }, enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    instance { a => enc.value.encode(genA.to(a)) }
}

object CsvEncoderMain extends App {
  def writeCsv[A](a: A)(implicit encA: CsvEncoder[A]): List[String] = encA.encode(a)

  sealed trait Shape
  final case class Rect(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  case class Employee(name: String, age: Int, manager: Boolean)

  println(writeCsv(Circle(2.0)))
  println(writeCsv(Employee("Roby",18,true)))

  val lst: List[Shape] = List(Rect(4.0, 5.0), Circle(7.0))
  println(writeCsv(lst)) //println(writeCsv(lst)(CsvEncoder.lstEncoder))

  case class Bar(baz: Int, qux: String)
  case class Foo(bar: Bar)
  println(writeCsv(Foo(Bar(10,"xxx"))))
}