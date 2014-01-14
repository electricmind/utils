name := "webcrawler"

version := "1.1"

scalaVersion := "2.10.2"

//mainClass := Option("ru.wordmetrix.WholeLotOfPictures")
mainClass := Some("ru.wordmetrix.webcrawler.WebCrawler")

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.2"

libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.5"

libraryDependencies +=  "com.typesafe.akka" %% "akka-actor" % "2.2.3"

libraryDependencies +=  "com.typesafe.akka" %% "akka-testkit" % "2.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M6" % "test"

//libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies +=  "org.scalacheck" %% "scalacheck" % "1.10.1"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.15.1"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

//libraryDependencies += "se.fishtank" % "css-selectors-scala_2.9.1" % "0.1.2"

//libraryDependencies += "commons-codec" % "commons-codec" % "1.2"
            
//resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

//org.scalastyle.sbt.ScalastylePlugin.Settings

//import de.johoop.findbugs4sbt.FindBugs._

//findbugsSettings

