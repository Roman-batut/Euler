import scala.math.Numeric.BigDecimalIsFractional
import scala.annotation.tailrec
import scala.util.boundary
import scala.collection.mutable.Map
import scala.io.Source

// def recognisePattern(s: String): (String, Boolean) = 
//     def empty(str: Array[String]): Boolean =
//         str.map(_.isEmpty()).foldRight(true)(_&&_)

//     @tailrec
//     def helper(str: String, acc: Int): String =
//         if str.size < 1 then str
//         else if empty(str.split(str.take(acc)).take(2)) then str.take(acc)
//         else helper(str, acc+1)
    
//     def helper2(acc: Int): (String, Boolean) =
//         if acc > 5 then (helper(s, 1), false)
        
//         val res = helper(s.drop(acc), 1)
//         if acc == 0 && s.size < 5 then (s, false)
//         else if res != s then (s.take(acc) ++ "(" + res + ")", true)
//         else helper2(acc+1)
    
//     helper2(0)
//     // val res = helper(s, 1)
//     // (res, res!=s)


// val str = (1d/8d).toString().split('.').last
// recognisePattern(str)

// (for 
//     i <- 2 to 100
// yield (BigDecimal(1)/BigDecimal(i)).toString().split('.').last).map(recognisePattern(_)).zipWithIndex.map((e, i) => (e, i+2))

def from(x: BigInt): LazyList[BigInt] = LazyList.cons(x, from(x + 1))
    
lazy val natural: LazyList[BigInt] = from(1)

def sqrt(number : BigInt) = {
    def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1

    val one = BigInt(1)

    var n = one
    var n1 = next(n, number)
    
    while ((n1 - n).abs > one) {
    n = n1
    n1 = next(n, number)
    }
    
    while (n1 * n1 > number) {
    n1 -= one
    }
    
    n1
}

def pow(number: BigInt, power: Int): BigInt =
    def helper(p: Int, acc: BigInt): BigInt =
        if p == 1 then acc
        else helper(p-1, acc*number)
    
    helper(power, number)

def isPrimeBigInt(n: BigInt): Boolean =
    @tailrec
    def helper(n: BigInt, acc: BigInt): Boolean =
        if acc >= sqrt(n)+1 then true
        else 
            if n%acc == 0 then false
            else helper(n, acc+1)

    if n <= 0 then false else helper(n, 2)

