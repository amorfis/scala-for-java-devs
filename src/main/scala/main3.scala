
extension(distanceInMeters: Double) def asMeters(): Meters = Meters(distanceInMeters)

@main
def main3(): Unit = {
  val distance = 10

  go(distance.asMeters())
}
