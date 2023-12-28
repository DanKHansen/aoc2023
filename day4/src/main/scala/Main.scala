@main def day4(): Unit =
    println(day4a)

def day4a =
    val ls: Iterator[String] = io.Source
        .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/4.txt")
        .getLines

    ls.map(l =>
        (
          l.substring(0, l.indexOf(":") + 1),
          l.substring(l.indexOf(":") + 1, l.indexOf("|"))
              .split(" ")
              .map(_.trim)
              .filterNot(_.isEmpty)
              .map(_.toInt),
          l.substring(l.indexOf("|") + 1)
              .split(" ")
              .map(_.trim)
              .filterNot(_.isEmpty)
              .map(_.toInt)
        )
    ).map(t => t._2.length - (t._2 diff t._3).length)
        .map(p => {
            if p == 0 then 0 else math.pow(2, p - 1)
        })
        .sum
    // )
