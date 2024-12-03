@main def day5(): Unit =
   println(s"1: ${seeds.map(findLocation(maps, _)).min}")
   println(s"2: ${findLocation(maps, seedRanges(seeds)).map(_.start).min}")

def sections: Array[String] =
   val file = "5.txt"
   val splitBy = "\r?\n\\s*\r?\n"
   val mainFolder = "/home/dankh/scalaprojects/aoc2023/sources/"
   val a = io.Source.fromFile(mainFolder + file)
   val ret = a.mkString.split(splitBy)
   a.close()
   ret

val seeds: Seq[Long] = sections(0) match
   case s"seeds: ${seedsStr}" => seedsStr.split(" ").map(_.toLong)
val maps: Maps = {
   val mapBuilder = Seq.newBuilder[Seq[MapEntry]]
   for (i <- 1 until sections.length)
      mapBuilder.addOne(
        sections(i)
           .split("\n")
           .tail
           .map { line =>
              val Array(dst, src, len) = line.split(" ")
              MapEntry(dst.toLong, src.toLong, len.toLong)
           }
           .toSeq)
   mapBuilder.result()
}

type Source = Long
type Destination = Long

case class MapEntry(destinationStart: Destination, sourceStart: Source, rangeLength: Long):
   def sourceEnd: Long = sourceStart + rangeLength
   def containsSource(value: Long): Boolean = sourceStart <= value && value < sourceEnd
   def distance: Long = destinationStart - sourceStart

type Maps = Seq[Seq[MapEntry]]

object SeedRange:
   def fromUntil(start: Long, end: Long): SeedRange = SeedRange(start, end - start)

case class SeedRange(start: Long, length: Long):
   def end: Long = start + length
   def contains(value: Long): Boolean = start <= value && value < end
   def translate(by: Long): SeedRange = SeedRange(start + by, length)

def mapping(map: Seq[MapEntry])(src: Source): Destination = map
   .find { case MapEntry(_, srcStart, len) => srcStart <= src && src < srcStart + len }
   .map { case MapEntry(dstStart, srcStart, _) => dstStart + (src - srcStart) }
   .getOrElse(src)

def findLocation(maps: Maps, seed: Source): Destination =
   maps.foldLeft[Source => Destination](identity)((acc, map) => acc andThen mapping(map))(seed)

def seedRanges(seeds: Seq[Long]): Set[SeedRange] =
   seeds.grouped(2).map { case Seq(start, length) => SeedRange(start, length) }.toSet

enum SplitResult:
   case Matched(seedRange: SeedRange, distance: Long)
   case Unmatched(seedRange: SeedRange)

def translateRange(usingMapEntries: Seq[MapEntry])(seedRange: SeedRange): Set[SeedRange] = {
   val res = scala.collection.mutable.Set.empty[SeedRange]
   val unmatchedRanges = scala.collection.mutable.Queue.empty[SeedRange]
   unmatchedRanges.addOne(seedRange)

   for mapEntry <- usingMapEntries do
      val unmatchedRangesNext = scala.collection.mutable.Set.empty[SeedRange]
      while unmatchedRanges.nonEmpty do
         val unmatchedRange: SeedRange = unmatchedRanges.removeHead(false)
         for splitResult <- calculatedSplits(mapEntry, unmatchedRange) do
            splitResult match
               case SplitResult.Matched(range, distance) =>
                  res.addOne(range.translate(distance))
               case SplitResult.Unmatched(unmatched) =>
                  if unmatchedRange != unmatched then unmatchedRanges.addOne(unmatched)
                  else unmatchedRangesNext.addOne(unmatched)
      unmatchedRanges.addAll(unmatchedRangesNext)

   res.addAll(unmatchedRanges)
   res.toSet
}

def calculatedSplits(mapEntry: MapEntry, seedRange: SeedRange): Seq[SplitResult] =
   if mapEntry.containsSource(seedRange.start) && mapEntry.containsSource(seedRange.end - 1) then
      Seq(SplitResult.Matched(seedRange, mapEntry.distance))
   else if mapEntry.containsSource(seedRange.start - 1) then
      Seq(
        SplitResult.Matched(SeedRange.fromUntil(seedRange.start, mapEntry.sourceEnd), mapEntry.distance),
        SplitResult.Unmatched(SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end))
      )
   else if mapEntry.containsSource(seedRange.end - 1) then
      Seq(
        SplitResult.Unmatched(SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart)),
        SplitResult.Matched(SeedRange.fromUntil(mapEntry.sourceStart, seedRange.end), mapEntry.distance)
      )
   else if seedRange.contains(mapEntry.sourceStart) && seedRange.contains(mapEntry.sourceEnd - 1) then
      Seq(
        SplitResult.Unmatched(SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart)),
        SplitResult.Matched(SeedRange.fromUntil(mapEntry.sourceStart, mapEntry.sourceEnd), mapEntry.distance),
        SplitResult.Unmatched(SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end))
      )
   else Seq(SplitResult.Unmatched(seedRange))

def findLocation(maps: Maps, seeds: Set[SeedRange]): Set[SeedRange] =
   maps.foldLeft(seeds) { case (ranges, mapEntries) => ranges.flatMap(translateRange(mapEntries)) }
