
case class Meters(value: Double)
case class Yards(value: Double)

given Conversion[Meters, Yards] = m => Yards(m.value * 1.094)

@main
def main2(): Unit = {
  val distance = Meters(10)

  go(distance)
}

def go(distance: Yards): Unit = {
  println(s"Going ${distance.value} yards")
}
