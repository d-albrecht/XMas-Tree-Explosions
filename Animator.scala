import scala.io.Source.fromFile

object Animator extends App {
	def narg(a: String, fb: Int): Int = args.toList.filter(_.startsWith(s"-$a=")).map(_.substring(3).toInt).headOption.getOrElse(fb)
	def targ(a: String, fb: String): String = args.toList.filter(_.startsWith(s"-$a=")).map(_.substring(3)).headOption.getOrElse(fb)
	def flag(f: String, fb: Boolean): Boolean = args.contains(s"-$f") ^ fb

	/* parameters */
	val seedingFrames = narg("l", 150)
	val seedingInterval = narg("i", 12)
	val expansionFrames = narg("e", 20)
	val fadingFrames = narg("f", 180)
	val expansionRadius = narg("r", 5) / 10.0
	val uniqueSeeds = !flag("b", false)
	val flicker = !flag("c", false)

	/* file input */
	val distsRaw = fromFile(targ("d", "dists.csv")).getLines.filter(_.size > 1).toList
	val dists = distsRaw.drop(1).map(_.split(",").map(_.toFloat).toList)
	val seedsRaw = fromFile(targ("s", "seeds.csv")).getLines.filter(_.size > 1).toList
	var seeds = seedsRaw.drop(1).map(_.split(",").map(_.toInt).toList)
	println("file input parsed")

	/* output file */
	val output = new java.io.PrintWriter(targ("o", "explosions.csv"))
	output.print("FRAME") // intentional not "FRAME_ID" to prevent sorting of already ordered rows
	dists.zipWithIndex.map(_._2).foreach(i => output.write(s",R_$i,G_$i,B_$i"))
	output.println()
	println("output file prepared")

	/* prepend last few seeds to the beginning */
	def rebase(seeds: List[List[Int]]): List[List[Int]] = {
		val timings = seeds.map(_(1)).sorted
		val gaps = timings.zip(timings.tail :+ (timings.head + seedingFrames))
		val newStart = gaps.map(g => (g._1, g._2, g._2 - g._1)).reverse.maxBy(_._3)._2 % seedingFrames
		println("shifting start to frame " + newStart)
		seeds.map(s => s.updated(1, (s(1) + seedingFrames - newStart) % seedingFrames))
	}
	@scala.annotation.tailrec def uniquify(seeds: List[List[Int]]): List[List[Int]] = {
		if (seeds.map(_(1)).distinct.size == seeds.size)
			seeds
		else {
			val shifted = seeds.sortBy(_(1)).foldLeft((-1, 0, List[List[Int]]())){
				case ((c, i, r), s) if (c < s(1)) => (s(1), i, s.updated(1, s(1) + i) :: r)
				case ((c, i, r), s) if (c == s(1)) => (c, i + 1, s.updated(1, c + i + 1) :: r)
			}
			uniquify(rebase(shifted._3))
		}
	}
	seeds = if(uniqueSeeds && seeds.size < seedingFrames / 2) uniquify(rebase(seeds)) else rebase(seeds)
	val loopingFrames = seedingFrames - (expansionFrames + fadingFrames) / seedingInterval - 2
	seeds = seeds ++ seeds.filter(_(1) > loopingFrames).map(s => s.updated(1, s(1) - seedingFrames))
	println("infinite looping prepared. number of seeds now: " + seeds.size)

	/* generating the actual color-table */
	def filterSeeds(frame: Int, d: List[Float])(s: List[Int]): Boolean = {
		s(1) * seedingInterval <= frame && // expansion phase		
		frame <= s(1) * seedingInterval + expansionFrames &&
		d(s(0)) <= expansionRadius * (frame - s(1) * seedingInterval) / expansionFrames ||
		s(1) * seedingInterval + expansionFrames <= frame && // fading phase
		frame <= s(1) * seedingInterval + expansionFrames + fadingFrames &&
		d(s(0)) <= expansionRadius
	}
	def dimmPattern(frame: Int)(s: List[Int]): List[Int] = { // corrupts index and the time frame information
		if (frame <= s(1) * seedingInterval + expansionFrames) {
			s
		} else {
			val flickering = scala.util.Random.nextFloat() * 0.125 * (if(flicker) 1 else 0)
			val dimming = 1.0 * (s(1) * seedingInterval + expansionFrames + fadingFrames - frame) / fadingFrames + flickering
			s.map(c => (c * dimming).toInt)
		}
	}
	var frame = 0
	for (seed <- 0 until seedingFrames) {
		output.flush() // don't keep too much in memory
		for (subframe <- 0 until seedingInterval) {
			output.print(frame)
			dists.foreach(d => {
				val patterns = List(0, 0, 0, 0, 0) :: seeds.filter(filterSeeds(frame, d)).map(dimmPattern(frame))
				output.print(patterns.transpose.drop(2).map(c => Math.min(255, c.sum)).mkString(",", ",", ""))
			})
			output.println()
			frame += 1
		}
	}
	println("color-table generated. frames generated: " + frame)

	/* finalize output */
	output.flush()
	output.close()
	println("animator is done")
}
