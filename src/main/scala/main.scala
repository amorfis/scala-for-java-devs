
case class Timeout(seconds: Int)

@main
def main(): Unit = {
  given timeout: Timeout = Timeout(3)

  val value = doSomething
  println(value)
}


def doSomething(using Timeout): String = {
  doSomethingDeeper
}

def doSomethingDeeper(using timeout: Timeout): String = {
  s"done in less than ${timeout.seconds} seconds"
}