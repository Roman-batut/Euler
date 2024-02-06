import scala.annotation.tailrec
import scala.util.boundary
import scala.collection.mutable.Map

object Utils {
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

    def fib(n: Int): BigInt =
        val cache: Map[Int, BigInt] = Map()
        def loop(idx: Int): BigInt =
            cache.getOrElseUpdate(
            idx,
            if idx <= 1 then 1
            else loop(idx - 1) + loop(idx - 2)
            )
        
        loop(n)
        
    def isPrimeBigInt(n: BigInt): Boolean =
        @tailrec
        def helper(n: BigInt, acc: BigInt): Boolean =
            if acc >= sqrt(n)+1 then true
            else 
                if n%acc == 0 then false
                else helper(n, acc+1)

        helper(n, 2)

    def isPrime(n: Int): Boolean =
        boundary:
            for i <- 1 to Math.sqrt(n).toInt do
                if n%i == 0 then boundary.break(false)
            true

    def getFirstPrimeFactor(n: BigInt, acc: BigInt): BigInt =
        if acc == n then 0
        else 
            if isPrimeBigInt(acc) && n%acc==0 then acc
            else getFirstPrimeFactor(n, acc+1)

    def getPrimeFactorisation(n: BigInt): List[BigInt] =
        val prime = getFirstPrimeFactor(n, 2)
        if isPrimeBigInt(n) then n :: Nil
        else prime :: getPrimeFactorisation(n/prime)


    def isPalindrome(s: String): Boolean =
        boundary:
            for i <- 0 to s.size/2 do
                if s.charAt(i) != s.charAt(s.size-1-i) then 
                    boundary.break(false)
            true

    def smallestDivisibleUntil(n: Int): Int = 
        def f(acc: Int): Boolean =
            boundary:
                for i <- 1 to n do
                    if acc%i != 0 then boundary.break(false)
                true

        def helper(acc: Int): Int =
            if f(acc) then acc
            else helper(acc+1)

        helper(1)
        
    def isSqrt(n: Double): Boolean =
        Math.sqrt(n).toInt == Math.sqrt(n)

    def getFactorList(n: BigInt): List[BigInt] =
        @tailrec
        def parser(acc: BigInt, list: List[BigInt]): List[BigInt] =
            if acc > n/2 then if n==1 then list else n :: list
            else if n%acc == 0 then parser(acc+1, acc::list)
            else parser(acc+1, list)

        parser(2, List(1))

    def factorial(n: BigInt): BigInt =
        @tailrec
        def helper(n: BigInt, acc: BigInt): BigInt =
            if n==1 then acc
            else helper(n-1, acc*n)
        helper(n, 1)

    def from(x: BigInt): LazyList[BigInt] = LazyList.cons(x, from(x + 1))
    
    lazy val natural: LazyList[BigInt] = from(1)
    
    lazy val d: LazyList[(BigInt, Int)] = natural.map(getFactorList(_).tail.foldRight(BigInt(0))(_+_)).zipWithIndex.map((e, i) => (e, i+1))
}
