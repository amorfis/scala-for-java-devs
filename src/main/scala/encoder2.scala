import learn.scala.example.{Address, Encoder, User}
import scala.compiletime.{constValue, erasedValue, summonInline}

@main
def runEncoder(): Unit = {
  val user = User("Pawel", 45, Address("Polska", "Szczecin", "Kwiatowa", 10))
  val encoded = Encoder.encode(user)
  println(encoded)
}
