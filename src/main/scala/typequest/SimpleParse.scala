package typequest

import scala.util.Try

import shapeless._, poly._, ops.hlist._, ops.nat._, UnaryTCConstraint._

import cats.Monoid
import cats.syntax.all._
import cats.std.all._


/**
 * Demonstrates histogramming of types that can be parsed from a single
 * column of a text file.
 *
 * To run, try it in the console:
 *   $ sbt console
 *   scala> import typequest.SimpleParse._
 *   scala> tvhExample
 *
 * In the example provided in the class, we have the following column of
 * string values:
 *   val column = List(
 *     "Hello", "42.0", "True", "False", "True", "True", "False", "True",
 *     "41", "42"
 *   )
 *
 * We attempt to parse each element using a parser that can recognize doubles
 * and a parser that can recognize strings:
 *   val parsers = doubleParser :: stringParser :: HNil
 * If parsing succeeds, we collect a histogram / map of all the unique values.
 *
 * In the case of this list, the output should be:
 *   tvhExample = Map(41.0 -> 1, 42.0 -> 2) ::
 *                Map(True  -> 4,
 *                    42    -> 1,
 *                    Hello -> 1,
 *                    41    -> 1,
 *                    False -> 2,
 *                    42.0  -> 1) ::
 *                HNil
 */
object SimpleParse {

  // --- Parsers

  /**
   * Parses a string to a T.
   *
   * A more sophisticated setup might use a sum type that could also capture
   * details about the parse failure, such as an Xor[FailureInfo, T].
   */
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
   *   forall T. (Parser[T], String) => Option[T]
   * It parses the string value with the given parser.
   */
  object parseValue extends Poly1 {
    implicit def go[T] = at[(Parser[T], String)] {
      case (parser, str) => parser.parse(str)
    }
  }

  /**
   * Runs each parser in an HList of parsers on a String, and return the
   * results concatenated into an HList.
   *
   * The type parameters of the output HList of options will match the type
   * parameters of the parsers. For example:
   *   Input:   Parser[Double] :: Parser[String] :: HNil
   *   Output:  Option[Double] :: Option[String] :: HNil
   */
  def runParsers [
    PS <: HList,
    H0 <: HList,
    H1 <: HList
  ](parserHList: PS)(value: String)
  (implicit
    parserConstraint: *->*[Parser]#λ[PS],
    constMapper: ConstMapper.Aux[String, PS, H0],
    zipper: Zip.Aux[PS :: H0 :: HNil, H1],
    mapper: Mapper[parseValue.type, H1]
  ): mapper.Out = {
    val cells = parserHList mapConst value
    (parserHList zip cells) map parseValue
  }

  val runParsersSample1 = runParsers(parsers)("42.0")
  val runParsersSample2 = runParsers(parsers)("Test")


  // --- Construct a single-element map for each parsed result

  /**
   * This is:
   *   forall T. Option[T] => Map[T, Long]
   */
  object toSingletonMap extends (Option ~> ({type L[T] = Map[T, Long]})#L) {
    def apply[T](x: Option[T]) = x match {
      case Some(y) => Map(y -> 1L)
      case None    => Map.empty
    }
  }

  val singletonMapExample = runParsers(parsers)("Test") map toSingletonMap


  // --- Monoid over HLists of Monoids

  /**
   * Provides a Cats / Algebra
   *   Monoid[T1 :: T2 :: ... :: TN :: HNil]
   * given Monoid instances for
   *          T1,   T2,   ...,   TN.
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
   * Creates typed histograms of successfully-parsed values in a list of
   * strings.
   *
   * The function should be supplied with an HList of parsers and a list of
   * strings to try parsing. Successful parse results are aggregated into
   * histograms (one per parser), which count the unique values that were
   * parsed.
   *
   * The function outputs maps from values to counts. The type parameters of
   * the output maps will match the type parameters of the parsers. For
   * example:
   *   Input:   Parser[Double]    :: Parser[String]    :: HNil
   *   Output:  Map[Double, Long] :: Map[String, Long] :: HNil
   */
  def typedValueHistograms [
    PS <: HList,
    H0 <: HList,
    H1 <: HList,
    H2 <: HList,
    H3 <: HList
  ](parserHList: PS)(column: List[String])
  (implicit
    parserConstrant: *->*[Parser]#λ[PS],
    ev1: ConstMapper.Aux[String, PS, H0],
    ev2: Zip.Aux[PS :: H0 :: HNil, H1],
    ev3: Mapper.Aux[parseValue.type, H1, H2],
    ev4: Mapper.Aux[toSingletonMap.type, H2, H3],
    ev5: Monoid[H3]
  ) = {
    column
      .map(value => runParsers(parserHList)(value) map toSingletonMap)
      .combineAll
  }

  val column = List(
    "Hello", "42.0", "True", "False", "True", "True", "False", "True",
    "41", "42"
  )
  val tvhExample = typedValueHistograms(parsers)(column)

}
