import scala.util.boundary

@main def hello(): Unit =
  var n = BigInt(0)
  var acc = BigInt(0)
  while n <= 10001 do
      if isPrimeBigInt(acc, 2) then 
          n+=1
          if n==10001 then println(acc)
      acc+=1

def isPrime(n: Int): Boolean =
    boundary:
        for i <- 1 to Math.sqrt(n).toInt do
            if n%i == 0 then boundary.break(false)
        true

def isPrimeBigInt(n: BigInt, acc: BigInt): Boolean =
    if acc == sqrt(n) then true
    else 
        if n%acc == 0 then false
        else isPrimeBigInt(n, acc+1)

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