object MITT extends App {
	println("Matt's Infotainment Two-Thousand")

	/* file input */
	val coords = scala.io.Source.fromFile("coords_2021.csv").getLines.filter(_.size > 1).toList.map(_.split(",")(2).toDouble)
	println("file input parsed")

	/* output file */
	val output = new java.io.PrintWriter("MITT.csv")
	output.print("FRAME_TIME")
	coords.zipWithIndex.map(_._2).foreach(i => output.write(s",R_$i,G_$i,B_$i"))
	output.println()
	println("output file prepared")

	/* calculate bulbs */
	val delta = .000001
	val (min, max) = coords.map(c => (c - delta, c + delta)).fold[(Double, Double)]((Double.PositiveInfinity, Double.NegativeInfinity))((l1, l2) => (Math.min(l1._1, l2._1), Math.max(l1._2, l2._2)))
	val bulbSize = ((max - min) / 8)
	val bulbed = coords.zipWithIndex.map{case (z, i) => {
		val zNorm = (z - min) / bulbSize
		i -> (zNorm.toInt, Math.abs(zNorm % 1 - .5))
	}}.toMap
	println("bulbs defined")

	/* frame buffer */
	val buffer = new Array[Int](coords.size)
	println("frame buffer prepared")
	def printBuffer(id: Int) {
		output.print(33)
		buffer.foreach(g => output.print(s",0,$g,0"))
		output.println()
	}
	def dimmBuffer() {
		bulbed.foreach{case (i, (b, d)) => buffer(i) = (buffer(i) * (0.925 - d * d / 2)).toInt}
	}
	def lightBuffer(bulb: Int, f: Int) {
		bulbed.filter(b => b._2._1 == bulb && b._2._2 * 6 <= f).foreach(b => buffer(b._1) = 255)
	}

	/* animation */
	var b = -1
	var f = 0
	for (i <- -35 until 70) {
		dimmBuffer()
		val bb = Math.abs(((i + 140) % 70) / 5 - 7)
		if (b == bb) f = f + 1 else f = 1
		b = bb
		lightBuffer(b, f)
		if (i >= 0) printBuffer(i)
	}

	/* finalize output */
	output.flush()
	output.close()
	println("MITT has departed")
}
