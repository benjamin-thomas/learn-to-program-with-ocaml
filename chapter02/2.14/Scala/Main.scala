/*

Run with ScalaCLI:

    scala-cli Main.scala

[Scala]$ time scala-cli ./Main.scala
Compiling project (Scala 3.5.1, JVM (17))
Compiled project (Scala 3.5.1, JVM (17))
4 Queens: 365596

real    0m27,671s
user    0m28,098s
sys     0m0,455s

 */

def solve(cols: Set[Int], d1: Set[Int], d2: Set[Int]): Int =
  if cols.isEmpty then 1
  else
    cols
      .diff(d1)
      .diff(d2)
      .foldLeft(0)((acc, n) =>
        acc + solve(cols - n, (d1 + n).map(_ + 1), (d2 + n).map(_ - 1))
      )

@main
def main(args: String*): Unit =
  println(
    "Possible solutions for 14 Queens: " + solve(
      Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
      Set(),
      Set()
    )
  )
