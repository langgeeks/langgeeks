fsc -classpath ./:lib/junit-4.8.1.jar:lib/scalatest-1.0.jar *.scala
scala -cp .:lib/scalatest-1.0.jar:lib/junit-4.8.1.jar  org.junit.runner.JUnitCore ReversiSuite
