import scala.util.matching.Regex
@main def day1(): Unit =
    println(s"one: $one, two: $two")

val input: List[String] = {
    val src = scala.io.Source
        .fromFile("/home/dankh/scalaprojects/aoc2023/sources/1.txt")
    val l = src.getLines().toList
    src.close()
    l
}
val mNum: Map[String, Int] = Map(
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9
)

val regEx: Regex =
    """(\d)|(?=(one|two|three|four|five|six|seven|eight|nine))""".r

def extractDigits(s: String): Int = {
    val ds: String = s.filter(_.isDigit)
    ds.length() match {
        case 0 => 0
        case _ => (ds.head.toString + ds.last.toString).toInt
    }
}
def extractNumbers(s: String): Int = {
    // Extracting a list of matches
    val m =
        regEx
            .findAllIn(s)
            .matchData
            .map(m =>
                (
                  m.start,
                  if m.matched == "" then 0 else m.matched.toInt,
                  mNum.getOrElse(m.group(1), 0),
                  mNum.getOrElse(m.group(2), 0)
                )
            )
            .toList
    // Getting a list of first and last digit
    val lDigits =
        m.head.toList.tail.filterNot(_ == 0).head :: m.last.toList.tail
            .filterNot(_ == 0)
    // Concatenating the list into a two-digit number
    lDigits.head.toString.concat(lDigits.last.toString).toInt
}
val one: Int = input.map(extractDigits).sum

val two: Int = input.map(extractNumbers).sum
