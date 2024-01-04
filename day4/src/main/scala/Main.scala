import scala.util.Using

lazy val wins: Seq[Int] = {
    val src = Using(
      io.Source.fromFile("/home/dankh/scalaprojects/aoc_2023/sources/4.txt")
    )(_.getLines.toList)

    src.get
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
        .map(t => t._2.intersect(t._1).length)

}

@main def run(): Unit =
    println(day4a(wins)) // 27454
    println(day4b(wins)) // 6857330

def day4a(input: Seq[Int]): Int =
    input.map(x => if x == 0 then 0 else math.pow(2, x - 1).toInt).sum

def day4b(input: Seq[Int]): Int =
    val queue = collection.mutable.Queue(1)
    // re-write to reduce instead of foldLeft
    input.foldLeft(0) { (idx, copies) =>
        val count = if (queue.isEmpty) 1 else queue.dequeue
        for (i <- 0 until copies) {
            if (i < queue.size) queue.update(i, queue(i) + count)
            else queue.append(count + 1)
        }
        idx + count
    }
