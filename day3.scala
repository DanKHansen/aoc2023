import scala.util.matching.Regex
private val regEx: Regex = """\d+""".r
val regEx2: Regex = """\*""".r

@main def day3(): Unit =
   println(s"1: $day3a")
   println(s"2: $day3b")

def day3a: Int =
   val a = io.Source.fromFile("/home/dankh/scalaprojects/aoc2023/sources/3.txt")
   val ls = a.getLines().toArray
   a.close()
   ls.map(l =>
      regEx
         .findAllIn(l)
         .matchData
         .map(m =>
            "%s%s%s".format(
              if ls.indexOf(l) - 1 > 0 then
                 ls(ls.indexOf(l) - 1).substring(
                   if (m.start < 1) m.start else m.start - 1,
                   if (m.end >= l.length) m.end else m.end + 1
                 )
              else "",
              ls(ls.indexOf(l)).substring(
                if (m.start < 1) m.start else m.start - 1,
                if (m.end >= l.length) m.end else m.end + 1
              ),
              if ls.indexOf(l) + 1 < ls.length - 1 then
                 ls(ls.indexOf(l) + 1).substring(
                   if (m.start < 1) m.start else m.start - 1,
                   if (m.end >= l.length) m.end else m.end + 1
                 )
              else ""
            )))
      .map(i =>
         i.map(s =>
            if (s.filter(_.!=('.')).forall(_.isDigit)) 0
            else regEx.findFirstMatchIn(s).get.matched.toInt)
            .sum)
      .sum

def day3b: Int =
   val a = io.Source
      .fromFile("/home/dankh/scalaprojects/aoc2023/sources/3.txt")
   val ls = a.getLines().toArray
   a.close()
   ls.flatMap(l =>
      regEx2
         .findAllIn(l)
         .matchData
         .map(m => // check if any number-candidates in adjacent cell

            (regEx
               .findAllMatchIn(ls(ls.indexOf(l) - 1))
               .filter(p1 =>
                  (p1.end - 1 == m.start - 1) || (p1.end - 1 == m.start) || (p1.end - 1 == m.start + 1) || (p1.start == m.start - 1) || (p1.start == m.start) || (p1.start == m.start + 1))
               .map(m => m.matched.toInt) ::
               regEx
                  .findAllMatchIn(ls(ls.indexOf(l)))
                  .filter(p => (p.end - 1 == m.start - 1) || (p.start == m.start + 1))
                  .map(m => m.matched.toInt) ::
               regEx
                  .findAllMatchIn(ls(ls.indexOf(l) + 1))
                  .filter(p =>
                     (p.end - 1 == m.start - 1) || (p.end - 1 == m.start) || (p.end - 1 == m.start + 1) || (p.start == m.start - 1) || (p.start == m.start) || (p.start == m.start + 1))
                  .map(m => m.matched.toInt) :: Nil).flatten)
         .map(xs => if xs.size == 2 then xs.product else 0))
      .sum
