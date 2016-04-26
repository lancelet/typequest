package typequest

import scala.util.Try

import shapeless._, poly._, ops.hlist._, ops.nat._, UnaryTCConstraint._
import cats.syntax.all._
import cats.std.all._

object SimpleParse {

  // --- Talk content

  case class Parser[T](parse: String => Option[T])

  val stringParser = Parser[String](Some.apply)

  val doubleParser = Parser[Double](s => Try(s.toDouble).toOption)

  val parsers = doubleParser :: stringParser :: HNil

  // --- Zipping the cell with the list

  val cell = "42.0"
  val cells = parsers mapConst cell
  val res0 = parsers zip cells

  // L :: R :: HNil
  // shapeless.::[L, shapeless.::[R, shapeless.HNil]]]

  /*
  def zipCell[L <: HList, M <: HList, R <: HList, O <: HList](cell: String, l: L)(
    implicit constMapper: ConstMapper.Aux[String, L, M], zipper: Zip.Aux[L :: M :: HNil, O]
  ) = {
    val cells = l mapConst cell
    l zip cells
  }
  */


  // --- Trying parsers with a cell

  object cellParse extends Poly1 {
    implicit def parse[T, S](implicit ev: S <:< (Parser[T], String)) = at[S] {
      case (parser: Parser[_], str: String) => parser.parse(str)
    }
  }

  def tryParsers [
    L <: HList,
    MapOut <: HList,
    ZipOut <: HList
  ](parsers: L, cell: String)(implicit
    parserConstraint: *->*[Parser]#λ[L],
    constMapper: ConstMapper.Aux[String, L, MapOut],
    zipper: Zip.Aux[L :: MapOut :: HNil, ZipOut],
    mapper: Mapper[cellParse.type, ZipOut]
  ) = {
    val cells = parsers mapConst cell
    (parsers zip cells) map cellParse
  }

  /*

  object cellParse extends Poly1 {
    implicit def parse[T, S](implicit ev: S <:< (Parser[T], String)) = at[S] {
      case (parser: Parser[_], str: String) => parser.parse(str)
    }
  }

  def tryParsers[L <: HList : *->*[Parser]#λ](parsers: L, cell: String) = {
    val parsersAndCells = zipCell(cell, parsers)
    parsersAndCells map cellParse
  }

  */


}