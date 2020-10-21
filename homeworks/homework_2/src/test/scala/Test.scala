import Exercises.Vector2D
import utest._

object Test extends TestSuite {
	
	val tests = Tests {
		'test_divBy3Or7 - {
			assert(Exercises.divBy3Or7(1, 3) == Seq(3))
			assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
			assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
		}
		'test_sumOfDivBy3Or5 - {
			assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
			assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
			assert(Exercises.sumOfDivBy3Or5(-10, 0) == -33)
			assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
			assert(Exercises.sumOfDivBy3Or5(3, 1) == 0)
		}
		'test_primeFactor - {
			assert(Exercises.primeFactor(228) == Seq(2, 3, 19))
			assert(Exercises.primeFactor(19) == Seq(19))
			assert(Exercises.primeFactor(1000000) == Seq(2, 5))
			assert(Exercises.primeFactor(1) == Seq.empty)
			assert(Exercises.primeFactor(2) == Seq(2))
			assert(Exercises.primeFactor(0) == Seq.empty)
			assert(Exercises.primeFactor(-295) == Seq(5, 59))
			assert(Exercises.primeFactor(-23) == Seq(23))
		}
		'test_sumScalars - {
			assert(Exercises.sumScalars(
				Vector2D(1, 1), Vector2D(2, 2),
				Vector2D(1, 1), Vector2D(2, 2)) == 8)
			assert(Exercises.sumScalars(
				Vector2D(0, 0), Vector2D(2, 24),
				Vector2D(12, 42), Vector2D(0, 0)) == 0)
			assert(Exercises.sumScalars(
				Vector2D(-1, -1), Vector2D(2, 2),
				Vector2D(-2, -2), Vector2D(2, 2)) == -12)
			assert(Exercises.sumScalars(
				Vector2D(1, 2), Vector2D(20, 20),
				Vector2D(-10, 15), Vector2D(2, 3)) == 85)
			assert(Exercises.sumScalars(
				Vector2D(0, 0), Vector2D(0, 0),
				Vector2D(0, 0), Vector2D(0, 0)) == 0)
		}
		'test_sumCosines - {
			assert(math.abs(Exercises.sumCosines(
				Vector2D(1, 1), Vector2D(2, 2),
				Vector2D(1, 1), Vector2D(2, 2))) - 2 < 1e-5)
			assert(math.abs(Exercises.sumCosines(
				Vector2D(-5, 1), Vector2D(2, -4),
				Vector2D(-10, -15), Vector2D(2, 2))) - 1.59452 < 1e-5)
			assert(math.abs(Exercises.sumCosines(
				Vector2D(10, 0), Vector2D(1, -10),
				Vector2D(20, 2), Vector2D(0, -20))) == 0)
			assert(Exercises.sumCosines(
				Vector2D(1, 1), Vector2D(0, 0),
				Vector2D(1, 1), Vector2D(-2, -2)).isNaN)
			assert(Exercises.sumCosines(
				Vector2D(0, 0), Vector2D(0, 0),
				Vector2D(0, 0), Vector2D(0, 0)).isNaN)
		}
		'test_sortByHeavyweight - {
			assert(Exercises.sortByHeavyweight(Exercises.balls) == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium",
				"Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium",
				"Lithium", "Magnesium", "Potassium", "Graphite" ))
			assert(Exercises.sortByHeavyweight(
				Map(
					"Aluminum" -> (5, 5),
					"Tungsten" -> (2, 2),
					"Gold" -> (3, 3)
				)) == Seq("Tungsten", "Gold", "Aluminum"))
			assert(Exercises.sortByHeavyweight(
				Map(
					"Aluminum" -> (1, 1),
					"Tungsten" -> (1, 1),
					"Gold" -> (1, 1)
				)) == Seq("Aluminum", "Tungsten", "Gold"))
			assert(Exercises.sortByHeavyweight(
				Map("Aluminum" -> (1, 1))) == Seq("Aluminum"))
			assert(Exercises.sortByHeavyweight(Map()) == Seq.empty)
		}
	}
}
