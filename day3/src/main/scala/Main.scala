import scala.util.matching.Regex

@main def day3: Unit =
    input.foreach(println)

    def input: Iterator[String] =
        io.Source
            .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/3a_ex.txt")
            .getLines()

    val regEx = """\d+""".r

    input.map(l => regEx.findAllMatchIn(l).foreach(println))
