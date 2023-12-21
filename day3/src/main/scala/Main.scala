import scala.util.matching.Regex
val regEx: Regex = """\d+""".r

@main def day3(): Unit =
    println(day3a)

def day3a: Int =
    val ls = io.Source
        .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/3a_ex.txt")
        .getLines()
        .toArray
    ls.map(l =>
        regEx
            .findAllIn(l)
            .matchData
            .map(m =>
                s"${
                        if ls.indexOf(l) - 1 > 0 then
                            ls(ls.indexOf(l) - 1).substring(
                              if (m.start < 1) m.start else m.start - 1,
                              if (m.end >= l.length) m.end else m.end + 1
                            )
                        else ""
                    }${ls(ls.indexOf(l)).substring(
                      if (m.start < 1) m.start else m.start - 1,
                      if (m.end >= l.length) m.end else m.end + 1
                    )}${
                        if ls.indexOf(l) + 1 < ls.length - 1 then
                            ls(ls.indexOf(l) + 1).substring(
                              if (m.start < 1) m.start else m.start - 1,
                              if (m.end >= l.length) m.end else m.end + 1
                            )
                        else ""
                    }"
            )
    ).map(i =>
        i.map(s =>
            if (s.filter(_.!=('.')).forall(_.isDigit)) 0
            else regEx.findFirstMatchIn(s).get.matched.toInt
        ).sum
    ).sum
