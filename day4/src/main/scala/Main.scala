lazy val wins: List[Int] =
    io.Source
        .fromFile("/home/dankh/scalaprojects/aoc_2023/sources/4.txt")
        .getLines
        .toList
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
@main def runday4(): Unit =
    println(day4a())
    println(day4b())

def day4a(): Int =
    wins.map(x => if x == 0 then 0 else math.pow(2, x - 1).toInt).sum

def day4b(): Int =
    val queue = collection.mutable.Queue(1)
    wins.foldLeft(0) { (sum, score) =>
        val count = if (queue.isEmpty) 1 else queue.dequeue()
        for (i <- 0 until score) {
            if (i < queue.size) queue.update(i, queue(i) + count)
            else queue.append(count + 1)
        }
        sum + count
    }
