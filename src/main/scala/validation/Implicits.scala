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

import scalaz._

object Implicits extends ValidationWithWarningsEnrichments {

  implicit def ValidationWithWarningsApplicative[L]: Applicative[({type l[a] = ValidationWithWarnings[L, a]})#l] =
    new Applicative[({type l[a] = ValidationWithWarnings[L, a]})#l] {
      override def map[A, B](fa: ValidationWithWarnings[L, A])(f: A => B) =
        fa map f

      def point[A](a: => A) =
        a.successNelWithoutWarnings

      def ap[A, B](va: => ValidationWithWarnings[L, A])(vfa: => ValidationWithWarnings[L, A => B]) =
        va.flatMap(a => vfa.map(f => f(a)))
    }

}
