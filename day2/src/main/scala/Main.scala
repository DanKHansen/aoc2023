import scala.util.matching.Regex
@main def day2(): Unit =
    println { two_a(input).sum }
    println { two_b(input).sum }

val regEx: Regex = """\s\d+\s\w+""".r
val regExId: Regex = """\d+""".r
val cubes: Map[String, Int] = Map("red" -> 12, "green" -> 13, "blue" -> 14)

def input: Iterator[String] =
    io.Source
        .fromFile("/home/dankh/scalaprojects/aoc2023/sources/2.txt")
        .getLines()

def two_a(in: Iterator[String]): Iterator[Int] =
    in
        .map(s =>
            regEx
                .findAllMatchIn(s)
                .map(m => m.matched.trim)
                .map(p => p.split(" "))
                .map(a =>
                    (regExId.findFirstIn(s).get.toInt, (a(0).toInt, a(1)))
                )
                .toArray
        )
        .map(a => {
            val id = a.head._1
            val maxR = a.groupBy(_._2._2)("red").maxBy(_._2._1)._2._1
            val maxG = a.groupBy(_._2._2)("green").maxBy(_._2._1)._2._1
            val maxB = a.groupBy(_._2._2)("blue").maxBy(_._2._1)._2._1
            if (
              maxR <= cubes("red") &&
              maxG <= cubes("green") &&
              maxB <= cubes("blue")
            )
                id
            else
                0
        })

def two_b(in: Iterator[String]): Iterator[Int] =
    input
        .map(s =>
            regEx
                .findAllMatchIn(s)
                .map(m => m.matched.trim)
                .map(p => p.split(" "))
                .map(a => ((a(0).toInt, a(1))))
                .toArray
        )
        .map(a => {
            val maxR = a.groupBy(_._2)("red").maxBy(_._1)._1
            val maxG = a.groupBy(_._2)("green").maxBy(_._1)._1
            val maxB = a.groupBy(_._2)("blue").maxBy(_._1)._1
            maxR * maxG * maxB
        })
