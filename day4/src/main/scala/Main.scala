import scala.collection.mutable
import scala.util.Using

@main def run(): Unit =
    println(day4a(wins)) // 27454
    println(day4b(wins)) // 6857330

private def wins: List[Int] = {
    Using(
      io.Source.fromFile("/home/dankh/scalaprojects/aoc_2023/sources/4.txt")
    )(_.getLines.toList).get
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

private def day4a(input: List[Int]): Int =
    input.map(x => if (x == 0) 0 else math.pow(2, x - 1).toInt).sum

private def day4b(input: List[Int]): Int =
    val queue = mutable.Queue(1)
    input.foldLeft(0) { (idx, copies) =>
        val count: Int = if (queue.isEmpty) 1 else queue.dequeue
        for (i <- 0 until copies)
            if (i < queue.size) queue.update(i, queue(i) + count)
            else queue.enqueue(count + 1)
        idx + count
    }
