fsc -classpath ./:lib/junit-4.8.1.jar:lib/scalatest-1.2.jar *.scala
scala -cp ./:lib/junit-4.8.1.jar:lib/scalatest-1.2.jar org.junit.runner.JUnitCore ReversiSuite
