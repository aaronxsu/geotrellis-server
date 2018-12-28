package geotrellis.server.example.ndvi


object ColorTest {

  def apply(tiles: List[(MultibandTile, Array[Histogram[Double]])],
            options: SingleBandOptions.Params): Option[MultibandTile] = {
    val band: Int = options.band
    val singleBandTiles = tiles map {
      case (tile: MultibandTile, _) =>
        tile.band(band)
    }

    if (singleBandTiles.isEmpty) {
      None
    } else {
      val hist: Histogram[Double] = tiles
        .map {
          case (_, histograms: Array[Histogram[Double]]) =>
            histograms.head
        }
        .reduce(_ merge _)
      val cmap = ColorRamps.Viridis

      val tile = cmap.render(
        singleBandTiles.reduce(_ merge _))
      val r = tile.map(_.red).interpretAs(UByteCellType)
      val g = tile.map(_.green).interpretAs(UByteCellType)
      val b = tile.map(_.blue).interpretAs(UByteCellType)
      val a = tile.map(_.alpha).interpretAs(UByteCellType)
      Some(MultibandTile(r, g, b, a))
    }
  }
}
