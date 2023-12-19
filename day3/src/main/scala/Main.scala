import scala.util.matching.Regex
@main def day3: Unit =
    println(s)

def ls =
    io.Source
        .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/3.txt")
        .getLines()
        .toArray

val regEx = """\d+""".r

val r: Array[Iterator[String]] =
    for l <- ls
    yield {
        for {
            m <- regEx.findAllIn(l).matchData
        } yield {
            {
                if (ls.indexOf(l) - 1 <= 0) ""
                else
                    ls(ls.indexOf(l) - 1).substring(
                      if (m.start < 1) m.start else m.start - 1,
                      if (m.end >= l.length) m.end else m.end + 1
                    )
            } + {
                ls(ls.indexOf(l)).substring(
                  if (m.start < 1) m.start else m.start - 1,
                  if (m.end >= l.length) m.end else m.end + 1
                )
            } + {
                if (ls.indexOf(l) + 1 >= ls.length - 1) ""
                else
                    ls(ls.indexOf(l) + 1).substring(
                      if (m.start < 1) m.start else m.start - 1,
                      if (m.end >= l.length) m.end else m.end + 1
                    )
            }
        }
    }

val s: Int = r
    .map(i =>
        i.map(s =>
            if (s.filter(_.!=('.')).filterNot(_.isDigit).length == 0) 0
            else regEx.findFirstMatchIn(s).get.matched.toInt
        ).sum
    )
    .sum
