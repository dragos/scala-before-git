
class Base extends Cloneable {
  override def clone = super.clone
}

@cloneable class BaseAttr {
}

object A extends Base  {
  object B extends Base
  class Z extends Base
}

object C {
  class Foo extends Base {
    override def clone = super.clone
  }
}

object D extends Cloneable {
  def clone(a: Object) = super.clone
}


object Test extends Application {

  def checkNoCloning(a: Base) {
    try {
      a.clone
    } catch {
      case _: CloneNotSupportedException => 
        println("checkNoCloning: ok")
    }
  }

  def checkCloning(a: Base) {
    val b = a.clone
    if (a.hashCode != b.hashCode)
      println("checkCloning: ok")
  }

  checkNoCloning(A)
  checkNoCloning(A.B)
  checkCloning(new C.Foo)
  checkCloning(new A.Z)
}
