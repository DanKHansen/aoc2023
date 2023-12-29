import scala.annotation.tailrec
val src: List[String] = io.Source
    .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/4a_ex.txt")
    .getLines
    .toList
@main def day4(): Unit =
    println(day4b(src))

def day4a(sc: Iterator[String]): Int =
    sc.map(l =>
        (
          l.substring(l.indexOf(":") + 1, l.indexOf("|"))
              .split(" ")
              .map(_.trim)
              .filterNot(_.isEmpty),
          l.substring(l.indexOf("|") + 1)
              .split(" ")
              .map(_.trim)
              .filterNot(_.isEmpty)
        )
    ).map(t => t._1.length - (t._1 diff t._2).length)
        .map(p => if p == 0 then 0 else math.pow(2, p - 1).toInt)
        .sum

def day4b(s: List[String]): Int =
    cards(winnerMap(s), winnerMap(s).keySet.toList, 1)

private def winnerMap(sc: List[String]) =
    val winners = sc
        .map(l =>
            (
              l.substring(l.indexOf(":") + 1, l.indexOf("|"))
                  .split(" ")
                  .map(_.trim)
                  .filterNot(_.isEmpty),
              l.substring(l.indexOf("|") + 1)
                  .split(" ")
                  .map(_.trim)
                  .filterNot(_.isEmpty)
            )
        )
        .map(t => t._1.length - (t._1 diff t._2).length)
    val origCard = Range.inclusive(1, winners.length).toList
    origCard.zip(winners).toMap

@tailrec
def cards(m: Map[Int, Int], l: List[Int], count: Int): Int =
    if (l.tail.isEmpty) count
    else
        cards(
          m,
          l.tail ++ Range.inclusive(
            l.head + 1,
            if (l.head + m(l.head) >= m.size) m.size
            else l.head + m(l.head)
          ),
          count + 1
        )
