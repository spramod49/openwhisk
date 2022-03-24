/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.openwhisk.core.entity


import scala.util.Failure
import scala.util.Success
import scala.util.Try

import spray.json._
// import org.apache.openwhisk.core.entity.size._
// import org.apache.openwhisk.core.ConfigKeys
// import pureconfig._

case class CudaMemoryLimitConfig(num: Int)

/**
 * GpuLimit encapsulates number of GPUs for an action.
 *
 * It is a value type (hence == is .equals, immutable and cannot be assigned null).
 * The constructor is private so that argument requirements are checked and normalized
 * before creating a new instance.
 *
 * @param num the number of GPUs for the action
 */
protected[entity] class CudaMemoryLimit private (val num: Int) extends AnyVal

protected[core] object CudaMemoryLimit extends ArgNormalizer[CudaMemoryLimit] {
//  val config = loadConfigOrThrow[CudaMemoryLimitConfig](ConfigKeys.cudamemory)

  protected[core] val numCudaMemory: Int = 4

  /** Gets CudaMemoryLimit with default value */
  protected[core] def apply(): CudaMemoryLimit = CudaMemoryLimit(numCudaMemory)

  /**
   * Creates CudaMemoryLimit for limit, iff limit is within permissible range.
   *
   * @param megabytes the limit in megabytes, must be within permissible range
   * @return MemoryLimit with limit set
   * @throws IllegalArgumentException if limit does not conform to requirements
   */
  @throws[IllegalArgumentException]
  protected[core] def apply(num: Int): CudaMemoryLimit = {
    require(num >= 1, s"Gpu $num below allowed threshold of 1")
    new CudaMemoryLimit(num)
  }

  override protected[core] implicit val serdes = new RootJsonFormat[CudaMemoryLimit] {
    def write(g: CudaMemoryLimit) = JsNumber(g.num)

    def read(value: JsValue) =
      Try {
        val JsNumber(num) = value
        require(num.isWhole(), "gpu must be whole number")
        CudaMemoryLimit(num.intValue)
      } match {
        case Success(limit)                       => limit
        case Failure(e: IllegalArgumentException) => deserializationError(e.getMessage, e)
        case Failure(e: Throwable)                => deserializationError("gpu number malformed", e)
      }
  }
}
