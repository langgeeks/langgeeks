import sbt._ 
 
class AProject(info: ProjectInfo) extends DefaultProject(info) { 

  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" %  "1.2.1-SNAPSHOT"
  // val specs = "org.scala-tools.testing" % "specs" %  "1.6.2"
  val junit = "junit" % "junit" % "4.8.2"

}
