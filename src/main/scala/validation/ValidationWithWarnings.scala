/**
 * Copyright 2016 https://github.com/Ajk4
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
**/

package pl.touk.util.validation

import scalaz.Scalaz._
import scalaz._

case class ValidationWithWarnings[+E, +A] private[validation](private val validation: ValidationNel[E, A],
                                                              warnings: Seq[E]) {
  import pl.touk.util.validation.Implicits._

  def flatMap[EE >: E, U](function: A => ValidationWithWarnings[EE, U]): ValidationWithWarnings[EE, U] = {
    validation match {
      case Success(value) =>
        val newResult = function(value)
        new ValidationWithWarnings(newResult.validation, warnings ++ newResult.warnings)
      case Failure(errors) => new ValidationWithWarnings[EE, U](errors.fail, warnings)
    }
  }

  def map[U](function: A => U): ValidationWithWarnings[E, U] = {
    val newValidation = validation.map(function)
    ValidationWithWarnings(newValidation, warnings)
  }

  def foreach[U](function: A => U): Unit = {
    validation.foreach(function)
  }

  def exists(f: A => Boolean): Boolean = validation.exists(f)

  def mapWarnings[EE >: E](function: E => EE) = {
    new ValidationWithWarnings(validation, warnings.map(function))
  }

  def mapErrors[EE >: E](function: E => EE): ValidationWithWarnings[EE, A] = {
    new ValidationWithWarnings(validation.leftMap(_.map(function)), warnings)
  }

  def mapErrorsAndWarnings[F](function: E => F): ValidationWithWarnings[F, A] = {
    new ValidationWithWarnings(validation.leftMap(_.map(function)), warnings.map(function))
  }
  
  def warningToErrors: ValidationWithWarnings[E, A] = {
    val warningsNelOpt = toNel(warnings.toList)
    (warningsNelOpt, validation) match {
      case (Some(warningsNel), Failure(errors)) => errors.append(warningsNel).failureWithoutWarnings
      case (Some(warningsNel), Success(_))      => warningsNel.failureWithoutWarnings
      case (None,              v)               => v.withWarnings()
    }
  }

  def ++[EE >: E, AA >: A : Semigroup](that: ValidationWithWarnings[EE, AA]) = {
    new ValidationWithWarnings(validation +++ that.validation, warnings ++ that.warnings)
  }

  def toOption: Option[A] = validation.toOption

  def valueOr[AA >: A](function: NonEmptyList[E] => AA): AA = validation.valueOr(function)

  def errors: Seq[E] = validation match {
    case Failure(errors) => errors.list
    case Success(_) => Seq()
  }

  def hasWarnings = warnings.nonEmpty
  
  def isSuccess = validation.isSuccess

  def isFailure = validation.isFailure

}