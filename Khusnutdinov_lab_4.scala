abstract class Summator[T] {
    def sum(lhs: T, rhs: T) : T
}

object Summator {
   def sum(lhs: T, rhs: T) : T = {
       implicitly[Summator[T]].cm(lhs, rhs)
   }
   implicit val int2Summator: Summator[Int] = new Summator[Int] {
       override def sum(lhs: Int, rhs: Int) : Int = {
            return lhs + rhs
       }
   }

   implicit val bool2Summator: Summator[Boolean] = new Summator[Boolean] {
       override def sum(lhs: Boolean, rhs: Boolean) : Boolean = {
            return lsh + rhs
       }
   }
}

class SuperNumber[T](num: T){
    var value = num

    def +(r: T)(implicit summator: Summator[T]) : SuperNumber[T]{
        var b = summator.sum(value, r)
        return new SuperNumber(b)
    }
}