package libraries.shapeless

import MigrationTest.IceCreamV1
import shapeless.LabelledGeneric
import cats.Monoid
import cats.instances.all._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Align, Diff, Intersection, Prepend}
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Lazy}



/**
  * @author Roberto Casadei
  *
  */

trait Migration[A,B] {
  def apply(a :A): B
}

object Migration {

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit m: Migration[A, B]): B = m.apply(a)
  }

  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] { def empty = zero; def combine(x:A, y:A): A = add(x,y) }

  implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil)((x, y) => HNil)

  implicit def emptyHList[K <: Symbol, H, T <: HList](implicit
                                                      hm: Lazy[Monoid[H]], tm: Monoid[T]): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hm.value.empty) :: tm.empty) {
      (x,y) => field[K](hm.value.combine(x.head, y.head)) :: tm.combine(x.tail, y.tail)
    }

  implicit def genericMigration[A, B, ARepr <: HList, BRepr <: HList, Common <: HList, Added <: HList, Unaligned<:HList]
  (implicit
   ga: LabelledGeneric.Aux[A,ARepr],
   gb: LabelledGeneric.Aux[B,BRepr],
   i: Intersection.Aux[ARepr,BRepr,Common],
   diff: Diff.Aux[BRepr, Common, Added],
   monoid: Monoid[Added],
   prepend: Prepend.Aux[Added, Common, Unaligned],
   align: Align[Unaligned,BRepr]
  ): Migration[A,B] =
    new Migration[A,B] {
      override def apply(a: A): B = gb.from(align(prepend(monoid.empty, i.apply(ga.to(a)))))
    }

}

object MigrationTest extends App {
  case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)
  // Remove fields:
  case class IceCreamV2a(name: String, inCone: Boolean)
  // Reorder fields:
  case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
  // Insert fields (provided we can determine a default value):
  case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)
  // Casoin
  case class IceCreamV3(newf: Double, inCone: Boolean, numWaffles: Int, name: String, numCherries: Int)

  import Migration._

  // Ideally we'd like to be able to write code like this:
  val rem = IceCreamV1("Sundae", 8, false).migrateTo[IceCreamV2a]
  val reorder = IceCreamV1("Sundae", 8, false).migrateTo[IceCreamV2b]
  val add = IceCreamV1("Sundae", 8, false).migrateTo[IceCreamV2c]
  val casoin = IceCreamV1("Sundae", 8, false).migrateTo[IceCreamV3]

  println(rem)
  println(reorder)
  println(add)
  println(casoin)

  val sundae = LabelledGeneric[IceCreamV1].to(IceCreamV1("Sundae", 1, false))
  println(sundae)
}