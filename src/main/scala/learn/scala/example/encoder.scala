package learn.scala.example

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

  def encode[A](a: A)(using enc: Encoder[A]): String = enc.encode(a)

  implicit inline def makeEncoder[A](using m: Mirror.Of[A]): Encoder[A] = (a: A) => {
    val labels: Seq[String] = deconstructLabels[m.MirroredElemLabels]

    val memberEncoders: Seq[Encoder[Any]] = deconstructMemberEncoders[m.MirroredElemTypes]

    val lines = a.asInstanceOf[Product].productIterator.zip(labels).zip(memberEncoders).map {
      case ((value, label), memberEncoder) =>
        s"$label: ${memberEncoder.encode(value)}"
    }

    constValue[m.MirroredLabel] + "\n" + lines.mkString("\n")
  }

  private inline def deconstructLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: (head *: tail) =>
        constValue[head].asInstanceOf[String] :: deconstructLabels[tail]
      case _: EmptyTuple => List.empty

  private inline def deconstructMemberEncoders[T <: Tuple]: List[Encoder[Any]] =
    inline erasedValue[T] match
      case _: (head *: tail) =>
        val encoder = summonInline[Encoder[head]]
        encoder.asInstanceOf[Encoder[Any]] :: deconstructMemberEncoders[tail]
      case _: EmptyTuple => List.empty
}
