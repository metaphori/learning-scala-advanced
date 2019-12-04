import FamilyPolyMain.semantics

object FamilyPolyMain extends App {
  println("Hello")

  class MySemantics extends Semantics {
    override type Context = ContextImpl
    override type O = Int

    class ContextImpl extends IContext {
      override def op: semantics.O = 88
    }
  }
  val semantics = new MySemantics()
  val interpreter = new semantics.Interpreter(new semantics.ContextImpl())
  println(interpreter.primitive1)
  println(interpreter.primitive2(interpreter.primitive1))
}

trait Core {
  type O
  type Context <: IContext

  trait IContext {
    def op: O
  }
}

trait Syntax { self: Core =>
  trait Constructs {
    def primitive1: O
    def primitive2(o: O): Int
  }
}

trait Semantics extends Core with Syntax {
  class Interpreter(c: Context) extends Constructs {
    override def primitive1: Semantics.this.O = c.op

    override def primitive2(o: Semantics.this.O): Int = 7
  }
}