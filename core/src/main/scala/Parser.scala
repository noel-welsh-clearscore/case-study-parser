/*
 * Copyright 2022 Creative Scala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package parser

import cats._
import scala.annotation.tailrec

sealed trait Parser[A] {
  import Parser._

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    ParserFlatMap(this, f)

  def map[B](f: A => B): Parser[B] =
    ParserMap(this, f)

  def and(that: => Parser[A])(implicit monoid: Monoid[A]): Parser[A] =
    ParserAnd(this, that, monoid)

  def orElse(that: => Parser[A]): Parser[A] =
    ParserOrElse(this, that)

  def product[B](that: Parser[B]): Parser[(A, B)] =
    ParserProduct(this, that)

  def repeat(implicit monoid: Monoid[A]): Parser[A] =
    ParserRepeat(this, monoid)

  def parse(input: String): Result[A] = {
    def loop[A](parser: Parser[A], index: Int): Result[A] =
      parser match {
        case ParserAnd(left, right, monoid) =>
          loop(left, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(resultL, _, offset) =>
              loop(right, offset) match {
                case Failure(reason, input, start) =>
                  Failure(reason, input, start)
                case Success(resultR, _, offset) =>
                  Success(monoid.combine(resultL, resultR), input, offset)
              }
          }

        case ParserFlatMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result, _, offset) =>
              val parser = f(result)
              loop(parser, offset)
          }

        case ParserMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result, input, offset) =>
              Success(f(result), input, offset)
          }

        case ParserOrElse(left, right) =>
          loop(left, index) match {
            case Failure(_, _, _) => loop(right, index)
            case Success(result, input, offset) =>
              Success(result, input, offset)
          }

        case ParserProduct(left, right) =>
          loop(left, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(resultA, _, offset) =>
              loop(right, offset) match {
                case Failure(reason, input, start) =>
                  Failure(reason, input, start)
                case Success(resultB, input, offset) =>
                  Success((resultA, resultB), input, offset)
              }
          }

        case ParserRepeat(source, monoid) =>
          def repeatLoop(result: A, index: Int): Result[A] = {
            loop(source, index) match {
              case Failure(_, input, start) =>
                Success(result, input, start)
              case Success(a, _, offset) =>
                repeatLoop(monoid.combine(result, a), offset)
            }
          }

          repeatLoop(monoid.empty, index)

        case ParserString(value) =>
          if (input.startsWith(value, index))
            Success(value, input, index + value.size)
          else
            Failure(
              s"input did not start with $value at index $index",
              input,
              index
            )

        case ParserPure(value) => Success(value, input, index)

        case ParserTailRecM(a, f) =>
          @tailrec def tailRecMLoop[A, B](
              a: A,
              f: A => Parser[Either[A, B]]
          ): Result[B] =
            loop(f(a), index) match {
              case Failure(reason, input, start) =>
                Failure(reason, input, start)
              case Success(either, input, offset) =>
                either match {
                  case Left(a)  => tailRecMLoop(a, f)
                  case Right(b) => Success(b, input, offset)
                }
            }

          tailRecMLoop(a, f)
      }

    loop(this, 0)
  }
}
object Parser {
  def string(value: String): Parser[String] = ParserString(value)
  def pure[A](value: A): Parser[A] = ParserPure(value)

  final case class ParserString(value: String) extends Parser[String]
  final case class ParserPure[A](value: A) extends Parser[A]
  final case class ParserRepeat[A](source: Parser[A], monoid: Monoid[A])
      extends Parser[A]
  final case class ParserFlatMap[A, B](source: Parser[A], f: A => Parser[B])
      extends Parser[B]
  final case class ParserOrElse[A](left: Parser[A], right: Parser[A])
      extends Parser[A]
  final case class ParserAnd[A](
      left: Parser[A],
      right: Parser[A],
      monoid: Monoid[A]
  ) extends Parser[A]
  final case class ParserProduct[A, B](left: Parser[A], right: Parser[B])
      extends Parser[(A, B)]
  final case class ParserMap[A, B](source: Parser[A], f: A => B)
      extends Parser[B]
  final case class ParserTailRecM[A, B](a: A, f: A => Parser[Either[A, B]])
      extends Parser[B]

  implicit val parserFunctorInstance: Monad[Parser] =
    new Monad[Parser] {
      override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
        fa.map(f)

      def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
        fa.flatMap(f)

      def pure[A](x: A): Parser[A] = Parser.pure(x)

      def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] =
        ParserTailRecM(a, f)
    }
}
