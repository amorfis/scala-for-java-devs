import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

case class User(name: String, age: Int, address: Address)

case class Address(country: String, city: String, street: String, number: Int)

trait Encoder[A] {
  def encode(a: A): String
}

object Encoder {
    given Encoder[String] with
        def encode(a: String): String = a

    given Encoder[Int] with
        def encode(a: Int): String = a.toString
}


inline implicit def makeEncoder[A](using m: Mirror.Of[A]): Encoder[A] = (a: A) => {
  val labels = deconstructLabels[m.MirroredElemLabels]

  val memberTypes = deconstructMemberTypes[m.MirroredElemTypes]

  val lines = a.asInstanceOf[Product].productIterator.zip(labels).zip(memberTypes).map {
    case ((value, label), show) => s"$label: ${show.encode(value)}"
  }

  constValue[m.MirroredLabel] + "\n" + lines.mkString("\n")
}

inline def deconstructLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
  case _: (head *: tail) => constValue[head].asInstanceOf[String] :: deconstructLabels[tail]
  case _: EmptyTuple => List.empty

inline def deconstructMemberTypes[T <: Tuple]: List[Encoder[Any]] = inline erasedValue[T] match
  case _: (head *: tail) =>
    val show = summonInline[Encoder[head]]
    show.asInstanceOf[Encoder[Any]] :: deconstructMemberTypes[tail]
  case _: EmptyTuple => List.empty

@main
def runEncoder(): Unit = {
  val v = User("Pawel", 45, Address("Polska", "Szczecin", "Kwiatowa", 10))
  val enc = summon[Encoder[User]]
  println(enc.encode(v))
}
