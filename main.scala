abstract class Summator[T] {
    def cm(p: List[Point], w: List[T]) : Point
}

object Summator {
   def cm[T: Summator](p: List[Point], w: List[T]) : Point = {
       implicitly[Summator[T]].cm(p, w)
   }
   implicit val int2Summator: Summator[Double] = new Summator[Double] {
       override def cm(p: List[Point], w: List[Double]) : Point = {
        var sum_x = 0.0
        var sum_y = 0.0
        var weights = 0.0   
        for (i <- 0 until p.length){
            sum_x += p(i).x * w(i)
            sum_y += p(i).y * w(i)
            weights += w(i)
        }
        return new Point(sum_x/weights, sum_y/weights)
       }
   }
}


class Point (x_coord: Double, y_coord: Double){
    var x = x_coord
    var y = y_coord
    var str = s"(${x_coord}, ${y_coord})"
}


class Triangle[T] (point1: Point, point2: Point, point3: Point, weight1: T, weight2: T, weight3: T) {
    val p1 = point1
    val p2 = point2
    val p3 = point3
    val w1 = weight1
    val w2 = weight2
    val w3 = weight3

    def perimeter() : Double = {
        val side1 = Math.sqrt(Math.pow((p2.x - p1.x), 2) - Math.pow((p2.y - p1.y), 2))
        val side2 = Math.sqrt(Math.pow((p3.x - p2.x), 2) - Math.pow((p3.y - p2.y), 2))
        val side3 = Math.sqrt(Math.pow((p3.x - p1.x), 2) - Math.pow((p3.y - p1.y), 2))
        return side1 + side2 + side3
    }

    def square() : Double = {
        return 0.5 * Math.abs((p2.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (p2.y - p1.y))
    }

    def countMassCenter()(implicit m: Summator[T]) : String = {
        val arrPoints = List(p1, p2, p3)
        val arrWeights = List(w1, w2, w3)
        var value1 = m.cm(arrPoints, arrWeights)
        return value1.str
    }
}

val tr1 = new Triangle(new Point(-1, -3), new Point(3, 4), new Point(5, -5), 1.0, 2.3, 3.1)
println(tr1.square())
println(tr1.countMassCenter())

val tr2 = new Triangle(new Point(-1, -3), new Point(3, 4), new Point(5, -5), "a", "b", "c")