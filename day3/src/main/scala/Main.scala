@main def run: Unit =
    input.foreach(println)

    def input: Iterator[String] =
        io.Source
            .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/3a_ex.txt")
            .getLines()
