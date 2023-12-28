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
              .map(_.toInt)
              .toList,
          l.substring(l.indexOf("|") + 1)
              .split(" ")
              .map(_.trim)
              .filterNot(_.isEmpty)
              .map(_.toInt)
              .toList
        )
    ).map(winners => winners._2.length - (winners._2 diff winners._3).length)
        .map(power => {
            if power == 0 then 0 else scala.math.pow(2, power - 1)
        })
        .sum
    // )
