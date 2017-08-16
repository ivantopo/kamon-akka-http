/*
 * =========================================================================================
 * Copyright © 2013-2017 the kamon project <http://kamon.io/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 * =========================================================================================
 */

val kamonCore        = "io.kamon" %% "kamon-core"            % "1.0.0-RC1-61029e115272b9af3f4460b311d3a2e650c806e3"
//val kamonAkka        = "io.kamon" %% "kamon-akka-2.4"        % "0.6.7"
//val kamonLogReporter = "io.kamon" %% "kamon-log-reporter"    % "0.6.7"

val http         = "com.typesafe.akka" %% "akka-http"          % "10.0.1"
val httpTestKit  = "com.typesafe.akka" %% "akka-http-testkit"  % "10.0.1"

lazy val root = (project in file("."))
  .aggregate(kamonAkkaHttp, kamonAkkaHttpPlayground)
  .settings(noPublishing: _*)
  .settings(Seq(crossScalaVersions := Seq("2.11.8", "2.12.1")))

lazy val kamonAkkaHttp = Project("kamon-akka-http", file("kamon-akka-http"))
  .settings(name := "kamon-akka-http")
  .settings(aspectJSettings: _*)
  .settings(Seq(
    scalaVersion := "2.12.1",
    crossScalaVersions := Seq("2.11.8", "2.12.1"),
    testGrouping in Test := singleTestPerJvm((definedTests in Test).value, (javaOptions in Test).value)))
  .settings(libraryDependencies ++=
    compileScope(http, kamonCore/*, kamonAkka*/) ++
      testScope(httpTestKit, scalatest, slf4jApi, slf4jnop) ++
      providedScope(aspectJ))

lazy val kamonAkkaHttpPlayground = Project("kamon-akka-http-playground", file("kamon-akka-http-playground"))
  .dependsOn(kamonAkkaHttp)
  .settings(Seq(
    scalaVersion := "2.12.1",
    crossScalaVersions := Seq("2.11.8", "2.12.1")))
  .settings(noPublishing: _*)
  .settings(settingsForPlayground: _*)
  .settings(libraryDependencies ++=
    compileScope(http, logbackClassic/*, kamonLogReporter*/) ++
    testScope(httpTestKit, scalatest, slf4jApi, slf4jnop) ++
    providedScope(aspectJ))

lazy val settingsForPlayground: Seq[Setting[_]] = Seq(
  connectInput in run := true,
  cancelable in Global := true
)

import sbt.Tests._
def singleTestPerJvm(tests: Seq[TestDefinition], jvmSettings: Seq[String]): Seq[Group] =
  tests map { test =>
    Group(
      name = test.name,
      tests = Seq(test),
      runPolicy = SubProcess(ForkOptions(runJVMOptions = jvmSettings)))
  }
