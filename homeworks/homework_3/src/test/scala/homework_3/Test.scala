package homework_3

import utest._

object Test extends TestSuite {
	
	val tests = Tests {
		'test_booleanFormatters - {
			val testData: Map[Any, String] = Map(
				12 -> "12",
				12.567 -> "12.567",
				"Hello!" -> "Hello!",
				'k' -> "k",
				Boolean -> "object scala.Boolean",
				Int -> "object scala.Int",
				true -> "правда",
				false -> "ложь")
			
			testPrettyBoolFormatters()
			
			def testPrettyBoolFormatters(): Unit = {
				for (elem <- testData) {
					assert(Exercises.prettyBooleanFormatter1(elem._1) == elem._2)
					assert(Exercises.prettyBooleanFormatter2(elem._1) == elem._2)
					assert(Exercises.prettyBooleanFormatter3(elem._1) == elem._2)
				}
			}
		}
		'test_findMaxElem - {
			val testData1: Map[Seq[Int], Int] = Map(
				Seq(1, 2, 3, 4, 5, 6, 7, 30) -> 30,
				Seq(0) -> 0,
				Seq(-1, -2912, -6129, -12) -> -1,
				Seq(-245, -125, 0, 125, 245) -> 245
			)
			val testData2: Map[Seq[Int], Seq[Int]] = Map(
				Seq(1, 2, 3, 4, 5, 6, 7, 30) -> Seq(30),
				Seq.empty -> Seq.empty,
				Seq() -> Seq.empty,
				Seq(0) -> Seq(0),
				Seq(-1, -2912, -6129, -12) -> Seq(-1),
				Seq(-245, -125, 0, 125, 245) -> Seq(245)
			)
			val testData3: Map[Seq[Int], Option[Int]] = Map(
				Seq(1, 2, 3, 4, 5, 6, 7, 30) -> Some(30),
				Seq.empty -> None,
				Seq() -> None,
				Seq(0) -> Some(0),
				Seq(-1, -2912, -6129, -12) -> Some(-1),
				Seq(-245, -125, 0, 125, 245) -> Some(245)
			)
			
			testFindMax()
			
			def testFindMax(): Unit = {
				testData1.foreach(test => assert(Exercises.max1(test._1) == test._2))
				testData2.foreach(test => assert(Exercises.max2(test._1) == test._2))
				testData3.foreach(test => assert(Exercises.max3(test._1) == test._2))
				
				try Exercises.max1(Seq.empty)
				catch {
					case e: Exception => println("Тест на пустую коллекцию пройден (выброшено исключение)")
				}
			}
		}
		'test_sumElements - {
			val testData: Map[(Int, Int), Int] = Map(
				(1, 2) -> 3,
				(-12, 13) -> 1,
				(-52, -48) -> -100,
				(0, 0) -> 0,
				(Int.MaxValue, Int.MaxValue) -> 2 * Int.MaxValue,
				(Int.MaxValue, Int.MinValue) -> -1
			)
			
			testFindSum()
			
			def testFindSum(): Unit = {
				for (test <- testData) {
					assert(Exercises.sum1(test._1._1, test._1._2) == test._2)
					assert(Exercises.sum2(test._1._1, test._1._2) == test._2)
					assert(Exercises.sum3(test._1._1, test._1._2) == test._2)
				}
			}
		}
	}
}