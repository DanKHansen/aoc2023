import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

@main
def main(): Unit =
    val input: Array[String] = almanac match
        case Failure(ex)    => Array(ex.getMessage)
        case Success(value) => value

    val seedList: Array[Long] = input.headOption match
        case Some(s"seeds: $value") =>
            value.split("\\s+").map(_.toLong)
        case _ => Array.emptyLongArray

    val seedRanges: List[LongRange] = seedList
        .grouped(2)
        .map(g => LongRange(g.head, g.head + g.last - 1))
        .toList
        .sortWith(_.first < _.first)

    val categoryMaps: Array[Array[RuleMap]] = input.tail.map:
        _.split("\r?\n").tail
            .map:
                _.split("\\s+")
                    .map(_.toLong) match {
                    case Array(d, s, l) => RuleMap(LongRange(d, s), l)
                    // dest, src, range
                }
            .sortBy(t => t.rng.last)

    val categoryMaps2: List[List[RuleMap]] = input.toList.tail.map:
        _.split("\r?\n").tail
            .map:
                _.split("\\s+")
                    .map(_.toLong) match {
                    case Array(d, s, l) =>
                        RuleMap(
                          LongRange(s, s + l - 1),
                          d - s
                        ) // src_start, src_end, mapping_rule to apply
                }
            .sortBy(t => t.rng.first)
            .toList

    val day5a: Array[Long] = seedList.map(s => recurse(s, categoryMaps))

    val day5b: List[List[LongRange]] = seedRanges.map(r => f(List(r), categoryMaps2))

    println(seedList.toList)
    println("*" * 6)
    seedRanges foreach println
    println("*" * 6)
    categoryMaps.map(_.toList).toList foreach println
    println("*" * 6)
    categoryMaps2 foreach println
    println("*" * 6)
    println(s"Day5a: ${day5a.toList.sorted.min}")
    println(
      s"Day5b: ${day5b.map(l => l.sortBy(_.first).minBy(_.first).first).min}"
    )

end main

def f(
    seedRngs: List[LongRange],
    catMaps: List[List[RuleMap]]
): List[LongRange] = {

    @tailrec
    def localF(
        seedRng: List[LongRange],
        ruleMaps: List[RuleMap]
    ): List[LongRange] = {

        if ruleMaps.isEmpty then f(seedRng, catMaps.tail)
        else
            (seedRng, ruleMaps) match
                case (in, rule) // option A
                    if in.last.first >= rule.head.rng.first && in.last.first <= rule.head.rng.last && in.last.last > rule.head.rng.last =>
                    localF(
                      List(
                        LongRange(
                          in.last.first + rule.head.rule,
                          rule.head.rng.last + rule.head.rule
                        ),
                        LongRange(rule.head.rng.last, in.last.last)
                      ),
                      ruleMaps.tail
                    )
                case (in, rule) // option B
                    if in.last.first < rule.head.rng.first && in.last.last <= rule.head.rng.last =>
                    f(
                      List(
                        in.head,
                        LongRange(in.last.first, rule.head.rng.first),
                        LongRange(
                          rule.head.rng.first + rule.head.rule,
                          in.last.last + rule.head.rule
                        )
                      ),
                      catMaps.tail
                    )
                case (in, rule) // Option C
                    if in.last.first >= rule.head.rng.first && in.last.last <= rule.head.rng.last =>
                    f(
                      List(
                        in.head,
                        LongRange(
                          in.last.first + rule.head.rule,
                          in.last.last + rule.head.rule
                        )
                      ),
                      catMaps.tail
                    )
                case (in, rule) // option D
                    if in.last.first < rule.head.rng.first && in.last.last > rule.head.rng.last =>
                    localF(
                      List(
                        LongRange(in.last.first, rule.head.rng.first - 1),
                        LongRange(
                          rule.head.rng.first + rule.head.rule,
                          rule.head.rng.last - 1 + rule.head.rule
                        ),
                        LongRange(rule.head.rng.last, in.last.last)
                      ),
                      ruleMaps.tail
                    )
                case (in, rule) // Option E/F
                    if in.last.first > rule.head.rng.last || in.last.last < rule.head.rng.first =>
                    localF(in, ruleMaps.tail)
                case (_, _) => f(seedRng, catMaps.tail)

    }
    if catMaps.nonEmpty then localF(seedRngs, catMaps.head)
    else seedRngs
}
def almanac: Try[Array[String]] =
    val file = "5.txt"
    val splitBy =
        "\r?\n\\s*\r?\n" // \r carriage return \n newline \s whitespace
    val mainFolder = "/home/dankh/scalaprojects/aoc_2023/sources/"
    util.Using(io.Source.fromFile(mainFolder + file))(
      _.mkString.split(splitBy)
    )
end almanac
@tailrec
def recurse(in: Long, l: Array[Array[RuleMap]]): Long =
    if l.isEmpty then in
    else
        l.head.flatMap(r => check(in, r)) match
            case Array(x) => recurse(x, l.tail)
            case _        => recurse(in, l.tail)
end recurse
def check(input: Long, r: RuleMap): Option[Long] =
    if input >= r.rng.last && input < r.rng.last + r.rule then
        Some(r.rng.first - r.rng.last + input)
    else None
end check
case class LongRange(first: Long, last: Long)
case class RuleMap(rng: LongRange, rule: Long)
