import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

case class User(name: String, age: Int)

case class Address(country: String, city: String, street: String, number: Int)

trait Show[A] {
  val typeName: String
  def show(a: A): String
}

object Show {
  given Show[String] with
    val typeName = "String"
    def show(a: String): String = a


  given Show[Int] with
    val typeName = "Int"
    def show(a: Int): String = a.toString
}

trait Encoder[A] {
  def encode(a: A): String
}

@main
def runEncoder(): Unit = {
  val v = Address("Polska", "Szczecin", "Kwiatowa", 10)
  val enc: Encoder[Address] = summon[Encoder[Address]]
  println(enc.encode(v))
}

inline implicit def makeEncoder[A](using m: Mirror.Of[A]): Encoder[A] = (a: A) => {
  val labels = deconstructLabels[m.MirroredElemLabels]

  val memberTypes = deconstructMemberTypes[m.MirroredElemTypes]

  val lines = a.asInstanceOf[Product].productIterator.zip(labels).zip(memberTypes).map {
    case ((value, label), show) => s"$label: ${show.show(value)}"
  }

  constValue[m.MirroredLabel] + "\n" + lines.mkString("\n")
}

inline def deconstructLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
  case _: (head *: tail) => constValue[head].asInstanceOf[String] :: deconstructLabels[tail]
  case _: EmptyTuple => List.empty

inline def deconstructMemberTypes[T <: Tuple]: List[Show[Any]] = inline erasedValue[T] match
  case _: (head *: tail) =>
    val show = summonInline[Show[head]]
    show.asInstanceOf[Show[Any]] :: deconstructMemberTypes[tail]
  case _: EmptyTuple => List.empty
