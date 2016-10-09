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

package validation

import org.scalatest.{Matchers, FlatSpec}
import pl.touk.util.validation._

import scalaz._
import scalaz.Scalaz._

import pl.touk.util.validation.Implicits._

class ValidationWithWarningsTest extends FlatSpec with Matchers {

  case class Person(age: Int)
  case class Group(people: Seq[Person])

  it should "return warnings if age = 20" in {
    val p = Person(20)
    val validation = checkAge(p)

    validation.isSuccess shouldBe true
    validation.warnings shouldEqual Seq("Illegal in some countries")
  }

  it should "not return no warnings if age = 22" in {
    val p = Person(22)
    val validation = checkAge(p)

    validation.isSuccess shouldBe true
    validation.hasWarnings shouldBe false
  }

  it should "join warnings if used in |@| " in {
    val v1 = checkAge(Person(20))
    val v2 = checkAge(Person(22))
    val v3 = checkAge(Person(20))

    val result = (v1 |@| v2 |@| v3) {
      case (p1,p2,p3) => Group(Seq(p1,p2,p3))
    }

    result.isSuccess shouldBe true
    result.warnings.size shouldEqual 2
  }

  it should "join warnings from successes with |@|, even if error occurred" in {
    val v1 = checkAge(Person(17))
    val v2 = checkAge(Person(22))
    val v3 = checkAge(Person(20))

    val result = (v1 |@| v2 |@| v3) {
      case (p1,p2,p3) => Group(Seq(p1,p2,p3))
    }

    result.isSuccess shouldBe false
    result.warnings.size shouldEqual 1
  }

  it should "carry warnings from leading successes, if failure occur in for-loop" in {
    val result = for {
      p1 <- checkAge(Person(20)) // generates warning
      p2 <- checkAge(Person(22)) // generates success
      p3 <- checkAge(Person(17)) // generates error. Already existings warnings should propagate
      p4 <- checkAge(Person(20)) // Skipped, since error occured
    } yield Group(Seq(p1,p2,p3, p4))

    result.isSuccess shouldBe false
    result.warnings.size shouldEqual 1
  }

  it should "possible to add VWW" in {
    val v1: ValidationWithWarnings[String, Int] = "Error message".failure.withWarning("Warning 1")
    val v2: ValidationWithWarnings[String, Int] = 1.success.withWarning("Warning 2")
    val v3: ValidationWithWarnings[String, Int] = 2.success.withWarning("Warning 3")

    val sum123 = v1 ++ v2 ++ v3
    sum123 match {
      case ValidationWithWarnings(Failure(NonEmptyList("Error message")), Seq("Warning 1", "Warning 2", "Warning 3")) =>
      case _ => fail
    }

    val sum23 = v2 ++ v3
    sum23 match {
      case ValidationWithWarnings(Success(3), Seq("Warning 2", "Warning 3")) =>
      case _ => fail
    }
  }

  def checkAge(p: Person): ValidationWithWarnings[String, Person]
  = if (p.age < 18)
    "Too Young!".failure.noWarnings
  else if (p.age > 40)
    "Too Old!".failure.noWarnings
  else if (p.age < 21)
    p.success.withWarning("Illegal in some countries")
  else
    p.success.noWarnings


}
