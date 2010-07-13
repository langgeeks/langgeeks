fsc -classpath ./:lib/junit-4.8.1.jar:lib/scalatest-1.2-for-scala-2.8.0.RC7-SNAPSHOT.jar *.scala
scala -cp ./:lib/junit-4.8.1.jar:lib/scalatest-1.2-for-scala-2.8.0.RC7-SNAPSHOT.jar org.junit.runner.JUnitCore ReversiSuite
