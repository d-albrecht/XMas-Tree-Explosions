import java.io.PrintWriter
import scala.io.Source

object Animator extends App {
	def param(n: Int, fb: String): String = {
		args.toList.drop(n).headOption.getOrElse(fb)
	}

	// parameters
	val seedingFrames = 100
	val seedingInterval = 15
	val expansionFrames = 20
	val fadingFrames = 150
	val expansionRadius = 0.5

	// file input
	val distsRaw = Source.fromFile(param(0, "dists.csv")).getLines.toList.filter(_.size > 1)
	val dists = distsRaw.drop(1).map(_.split(",").drop(1).map(_.toFloat)).map(_.toList)
	val seedsRaw = Source.fromFile(param(1, "seeds.csv")).getLines.toList.filter(_.size > 1)
	var seeds = seedsRaw.drop(1).map(_.split(",").map(_.toInt)).map(_.toList).map(s => s.updated(0, s(0) - 1))
	println("file input parsed")

	// output file
	val output = new PrintWriter(param(2, "explosions.csv"))
	output.print("frame")
	dists.zipWithIndex.map(_._2).foreach(i => output.write(s",r_$i,g_$i,b_$i"))
	output.println()
	println("output file prepared")

	// prepend last few seeds to the beginning
	val loopingFrames = seedingFrames - (expansionFrames + fadingFrames) / seedingInterval - 2
	seeds = seeds ++ seeds.filter(_(1) > loopingFrames).map(s => s.updated(1, s(1) - seedingFrames))
	println("infinite looping prepared. number of seeds now: " + seeds.size)

	// generatin the actual color-table
	def filterSeeds(frame: Int, d: List[Float])(s: List[Int]): Boolean = {
		// expansion phase		
		s(1) * seedingInterval < frame &&
		frame <= s(1) * seedingInterval + expansionFrames &&
		d(s(0)) <= expansionRadius * (frame - s(1) * seedingInterval) / expansionFrames ||
		// fading phase
		s(1) * seedingInterval + expansionFrames <= frame &&
		frame < s(1) * seedingInterval + expansionFrames + fadingFrames &&
		d(s(0)) <= expansionRadius
	}
	// this helper function will corrupt both the index and the time frame information, don't rely on them anymore
	def dimmPattern(frame: Int)(s: List[Int]): List[Int] = {
		if (frame <= s(1) * seedingInterval + expansionFrames) {
			s
		} else {
			val dimming = 1.0 * (s(1) * seedingInterval + expansionFrames + fadingFrames - frame) / fadingFrames
			s.map(c => (c * dimming).toInt)
		}
	}
	var frame = 0
	for (seed <- 0 until seedingFrames) {
		println("generating seeding frame " + seed)
		for (subframe <- 0 until seedingInterval) {
			output.print(frame)
			dists.foreach(d => {
				val patterns = seeds.filter(filterSeeds(frame, d))
				val dimmed = patterns.map(dimmPattern(frame))
				val r = dimmed.map(_(2)).sum
				val g = dimmed.map(_(3)).sum
				val b = dimmed.map(_(4)).sum
				output.print(s",${Math.min(255,r)},${Math.min(255,g)},${Math.min(255,b)}")
			})
			frame += 1
			output.println()
		}
	}
	println("color-table generated. frames generated: " + frame)

	// flush and close output
	output.flush()
	output.close()
	println("animator is done")
}
