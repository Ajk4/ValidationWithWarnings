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

trait ValidationWithWarningsEnrichments {

  implicit class ValidationNelOps[E, A](validation: ValidationNel[E, A]) {
    def withWarnings(warnings: Seq[E] = Nil): ValidationWithWarnings[E, A] = ValidationWithWarnings(validation, warnings)
    def withWarning(warning: E): ValidationWithWarnings[E, A] = ValidationWithWarnings(validation, Seq(warning))
    def noWarnings: ValidationWithWarnings[E, A] = ValidationWithWarnings(validation, Nil)
  }


  implicit class ValidationOps[E, A](validation: Validation[E, A]) {
    def withWarnings(warnings: Seq[E] = Nil): ValidationWithWarnings[E, A] =
      new ValidationNelOps(validation.toValidationNel).withWarnings(warnings)

    def withWarning(warning: E): ValidationWithWarnings[E, A] =
      new ValidationNelOps(validation.toValidationNel).withWarning(warning)

    def noWarnings: ValidationWithWarnings[E, A] = new ValidationNelOps(validation.toValidationNel).noWarnings
  }

  implicit class ValidationWithWarningsSuccessHelper[A](value: A) {
    def successNelWithoutWarnings[E]: ValidationWithWarnings[E, A] = new ValidationWithWarnings(value.successNel[E], Seq())
  }

  implicit class ValidationWithWarningsFailureNelHelper[E](error: NonEmptyList[E]) {
    def failureWithoutWarnings[A]: ValidationWithWarnings[E, A] = new ValidationWithWarnings(error.failure[A], Seq())
  }

  implicit class ValidationWithWarningsFailureHelper[E](error: E) {
    def failureNelWithoutWarnings[A]: ValidationWithWarnings[E, A] = new ValidationWithWarnings(error.failureNel[A], Seq())
  }

  implicit class SeqOfValidationWithWarnings[E, A](validationSeq: Seq[ValidationWithWarnings[E, A]]) {

    def accumulateValidationsIgnoringValue: ValidationWithWarnings[E, Unit] = accumulateValidations.map(_ => ())

    /**
     * Converts Seq[VWW[E,A]] to VWW[E,[Seq[A]]
     */
    def accumulateValidations: ValidationWithWarnings[E, Seq[A]] = {
      if (validationSeq.isEmpty)
        Seq().successNelWithoutWarnings
      else
        accumulateValidationsNotEmpty
    }

    private def accumulateValidationsNotEmpty: ValidationWithWarnings[E, Seq[A]] = {
      validationSeq.map { singleValidation =>
        val validationOfLists = singleValidation.map(singletonValue => List(singletonValue))
        validationOfLists
      }.reduce(_ ++ _)
    }
  }

}
