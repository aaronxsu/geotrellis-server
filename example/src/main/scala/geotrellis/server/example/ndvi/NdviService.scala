package geotrellis.server.example.ndvi

import geotrellis.server.vlm.gdal.GDALNode
import geotrellis.server._
import geotrellis.raster._
import geotrellis.raster.render._
import com.azavea.maml.ast._
import com.azavea.maml.ast.codec.tree._
import com.azavea.maml.eval._

import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import _root_.io.circe._
import _root_.io.circe.parser._
import _root_.io.circe.syntax._
import cats.data._
import Validated._
import cats.effect._
import com.typesafe.scalalogging.LazyLogging

import java.net.URLDecoder

class NdviService[Param](
  interpreter: BufferingInterpreter = BufferingInterpreter.DEFAULT
)(implicit contextShift: ContextShift[IO],
           enc: Encoder[GDALNode],
           dec: Decoder[GDALNode],
           mr: TmsReification[GDALNode]) extends Http4sDsl[IO] with LazyLogging {

  object ParamBindings {
    def unapply(str: String): Option[Map[String, GDALNode]] =
      decode[Map[String, GDALNode]](str) match {
        case Right(res) => Some(res)
        case Left(_) => None
      }
  }

  implicit val redQueryParamDecoder: QueryParamDecoder[GDALNode] =
    QueryParamDecoder[String].map { str => decode[GDALNode](URLDecoder.decode(str, "UTF-8")).right.get }
  //object RedQueryParamMatcher extends QueryParamDecoderMatcher[Param]("red")
  //object NirQueryParamMatcher extends QueryParamDecoderMatcher[Param]("nir")

  implicit val expressionDecoder = jsonOf[IO, Expression]

  final val ndvi: Expression =
    Division(List(
      Subtraction(List(
        RasterVar("red"),
        RasterVar("nir"))),
      Addition(List(
        RasterVar("red"),
        RasterVar("nir"))
      ))
    )

  final val eval = LayerTms.curried(ndvi, interpreter)

  // http://0.0.0.0:9000/{z}/{x}/{y}.png
  def routes: HttpRoutes[IO] = HttpRoutes.of {
    // Matching json in the query parameter is a bad idea.
    case req @ GET -> Root / IntVar(z) / IntVar(x) / IntVar(y) ~ "png" =>
      //val uri = new java.net.URI("s3://rasterfoundry-staging-data-us-east-1/public-cogs/ece3fcc0-dbdb-41b9-a364-73759c631771_COG.tif")
      //val uri = new java.net.URI("https://rasterfoundry-staging-data-us-east-1.s3.amazonaws.com/public-cogs/ece3fcc0-dbdb-41b9-a364-73759c631771_COG.tif?AWSAccessKeyId=AKIAIENVM555U4G665PA&Signature=TA5EtLXRImPWKPkw2O%2FFOXmmAyc%3D&Expires=1545339608")
      val uri = new java.net.URI("https://s3.amazonaws.com/geotrellis-test/npz/testtiffs/clipped.tif")
      val paramMap: Map[String, GDALNode] = Map("red" -> GDALNode(uri, 1, None), "nir" -> GDALNode(uri, 2, None))

      println("got param map")
      eval(paramMap, z, x, y).attempt flatMap {
        case Right(Valid(mbtile)) =>
          ColorTest(List((mbtile, Array(mbtile.band(0).histogram))))
          // Image results have multiple bands. We need to pick one
          Ok(mbtile.band(0).renderPng(ColorRamps.Viridis).bytes)
        case Right(Invalid(errs)) =>
          logger.debug(errs.toList.toString)
          BadRequest(errs.asJson)
        case Left(err) =>
          logger.debug(err.toString, err)
          InternalServerError(err.toString)
      }
  }
}

