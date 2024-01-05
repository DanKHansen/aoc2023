import scala.util.{Failure, Success, Try}

@main
def main(): Unit = {
    val input: Seq[String] = almanac match
        case Failure(ex)    => List(ex.getMessage)
        case Success(value) => value
}
final def almanac: Try[List[String]] =
    val file = "5a_ex.txt"
    val mainFolder = "/home/dankh/scalaprojects/aoc_2023/sources/"
    util.Using(io.Source.fromFile(mainFolder + file))(
      _.mkString.split("\r?\n\\s*\r?\n").toList
    )
