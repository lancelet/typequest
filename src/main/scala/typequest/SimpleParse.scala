package typequest

import scala.util.Try

import shapeless._, poly._, ops.hlist._, ops.nat._, UnaryTCConstraint._
import cats.Monoid
import cats.syntax.all._
import cats.std.all._

object SimpleParse {

  // --- Parsers

  /** Parses a string to a T. */
  case class Parser[T](parse: String => Option[T])

  val stringParser = Parser[String](Some.apply)

  val doubleParser = Parser[Double](s => Try(s.toDouble).toOption)

  /** HList of parsers we want to use later. */
  val parsers = doubleParser :: stringParser :: HNil

  // --- Try multiple parsers on a single value

  /**
   * Parses a single value.
   *
   * This is essentially:
   *   (Parser[T], String) -> Option[T]
   *
   * It takes a tuple of a parser and a String value to attempt to parse. It
   * returns the result of the parsing.
   */
  object parseValue extends Poly1 {
    implicit def go[T, S](implicit ev: S <:< (Parser[T], String)) = at[S](x =>
      ev(x) match {
        case (parser, str) => parser.parse(str)
      }
    )
  }

  /**
   * Runs each parser in an HList of parsers on a String, and return the
   * results concatenated into an HList.
   *
   * The type parameters of the input parsers will match the type parameters
   * of the output parse results. So, for example, if the parser types are as
   * follows:
   *   `Parser[Double] :: Parser[String] :: HNil`
   * then the returned type will be:
   *   `Option[Double] :: Option[String] :: HNil`
   */
  def runParsers [
    L <: HList,
    H0 <: HList,
    H1 <: HList
  ](parserHList: L)(value: String)
  (implicit
    parserConstraint: *->*[Parser]#λ[L],
    constMapper: ConstMapper.Aux[String, L, H0],
    zipper: Zip.Aux[L :: H0 :: HNil, H1],
    mapper: Mapper[parseValue.type, H1]
  ): mapper.Out = {
    val cells = parserHList mapConst value
    (parserHList zip cells) map parseValue
  }

  val runParsersSample1 = runParsers(parsers)("42.0")
  val runParsersSample2 = runParsers(parsers)("Test")

  // --- Construct a single-element map for each parsed result

  /** This performs `Option[T] -> Map[T, Long]` (forall `T`). */
  object toSingletonMap extends (Option ~> ({type L[T] = Map[T, Long]})#L) {
    def apply[T](x: Option[T]) = x match {
      case Some(y) => Map(y -> 1L)
      case None    => Map.empty
    }
  }

  val singletonMapExample = runParsersSample2 map toSingletonMap

  // --- Monoid over HLists of Monoids

  /**
   * Provides a `Monoid[T1 :: T2 :: ... :: TN :: HNil]` given `Monoid`
   * instances for `T1`, `T2`, ... `TN`.
   *
   * (transcribed from the shapeless example `monoids.scala`, but for Cats
   * Monoids)
   */
  object HLMonoid extends ProductTypeClassCompanion[Monoid] {
    object typeClass extends ProductTypeClass[Monoid] {
      def emptyProduct =
        new Monoid[HNil] {
          def empty = HNil
          def combine(a: HNil, b: HNil) = HNil
        }
      def product[F, T <: HList](mh: Monoid[F], mt: Monoid[T]) =
        new Monoid[F :: T] {
          def empty = mh.empty :: mt.empty
          def combine(a: F :: T, b: F :: T) =
            mh.combine(a.head, b.head) :: mt.combine(a.tail, b.tail)
        }
      def project[F, G](instance: => Monoid[G], to: F => G, from: G => F) =
        new Monoid[F] {
          def empty = from(instance.empty)
          def combine(a: F, b: F) = from(instance.combine(to(a), to(b)))
        }
    }
  }

  import HLMonoid._

  // --- Summarize a column of input values

  /**
   * Monoidal aggregation of the product of parsers applied to string values
   * in a list.
   *
   * For each string in the list, attempt parsing using all provided parsers.
   * Then aggregate all results, to produce maps between values of parsed
   * types and their counts.
   *
   * Again, the type parameters of the input parsers will match the type
   * parameters of the output maps. For example, if the input parsers are:
   *   Parser[Double] :: Parser[String] :: HNil
   * Then the output maps will be:
   *   Map[Double, Long] :: Map[String, Long] :: HNil
   */
  def summarize [
    L <: HList,
    H0 <: HList,
    H1 <: HList,
    H2 <: HList,
    H3 <: HList
  ](parserHList: L)(column: List[String])
  (implicit
    parserConstrant: *->*[Parser]#λ[L],
    ev1: ConstMapper.Aux[String, L, H0],
    ev2: Zip.Aux[L :: H0 :: HNil, H1],
    ev3: Mapper.Aux[parseValue.type, H1, H2],
    ev4: Mapper.Aux[toSingletonMap.type, H2, H3],
    ev5: Monoid[H3]
  ): H3 = {
    column
      .map(value => runParsers(parserHList)(value) map toSingletonMap)
      .combineAll
  }

  val column = List(
    "Hello", "42.0", "True", "False", "True", "True", "False", "True"
  )
  val summarizeExample = summarize(parsers)(column)

}
