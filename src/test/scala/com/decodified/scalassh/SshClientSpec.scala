/*
 * Copyright (C) 2011 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.decodified.scalassh

import org.specs2.Specification
import java.io.File
import io.Source
import org.specs2.execute.{Failure, FailureException}

class SshClientSpec extends Specification { def is =

  "The SshClient should be able to" ^
    "properly connect to the test host and fetch a directory listing"           ! simpleTest^
    "properly connect to the test host and execute screen"           ! screenTest^
    "properly connect to the test host and execute three independent commands"  ! threeCommandsTest

  def getResult[T](ssh: Validated[T]): T = {
    println(ssh)
    ssh.right.get
  }

  def simpleTest = {
    getResult(SSH(testHostName) {
      client =>
        client.exec("ls -a").right.map {
          result =>
            result.stdOutAsString() + "|" + result.stdErrAsString()
        }
    }) must startWith(".\n..\n")
  }

  def screenTest = {
    getResult(SSH(testHostName, new HostConfigProvider {
      def apply(v1: String) = HostFileConfig()(v1).right.flatMap(c => Right(c.copy(allocatePTY = HostConfig.Cygwin)))
    }) {
      client =>
        client.exec("screen echo ololo").right.map {
          result =>
            result.stdOutAsString() + "|" + result.stdErrAsString()
        }
    }) must contain("olo")
  }

  def threeCommandsTest = {
    getResult(SSH(testHostName) { client =>
      client.exec("ls").right.flatMap { res1 =>
        println("OK 1")
        client.exec("dfssgsdg").right.flatMap { res2 =>
          println("OK 2")
          client.exec("uname").right.map { res3 =>
            println("OK 3")
            (res1.exitCode, res2.exitCode, res3.exitCode)
          }
        }
      }
    }) mustEqual (Some(0), Some(127), Some(0))
  }

  lazy val testHostName = {
    val fileName = HostFileConfig.DefaultHostFileDir + File.separator + ".testhost"
    try {
      Source.fromFile(fileName).getLines().toList.head
    } catch {
      case e: Exception => throw FailureException(Failure(("Could not find file '%s', you need to create it holding " +
        "nothing but the name of the test host you would like to run your tests against!").format(fileName), e.toString))
    }
  }
}
