def digitOfChar(c: Char): Int =
  c match {
    case it if it >= '0' && it <= '9' => it.toInt - 48
    case it if it >= 'A' && it <= 'Z' => it.toInt - 55
    case _ => throw new IllegalArgumentException(s"Invalid digit: $c")
  }

@main def hello(): Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
