import scala.math.Numeric.BigDecimalIsFractional
import scala.annotation.tailrec
import scala.util.boundary
import scala.collection.mutable.Map
import scala.io.Source

import Utils.*

class Archives:

    def first =
        (for 
            i <- 1 to 999
            if i%3==0 || i%5==0 
        yield i).foldRight(0)(_+_) 

    def second = 
        (for 
            i <- 1 to 32
            if fib(i)%2==0
        yield fib(i))
        .foldRight(BigInt(0))(_+_)

    def third = 
        getPrimeFactorisation(BigInt("600851475143"))

    def fourth = 
        (for 
            i <- 100 to 999
            j <- i to 999 
            if isPalindrome((i*j).toString())
        yield i*j).max

    def fifth = smallestDivisibleUntil(20)

    def sixth = 
        val x1 = Math.pow((
            (for 
                i <- 1 to 100
            yield i).foldRight(0)(_+_)
        ), 2) 
        val x2 = (for 
            i <- 1 to 100
        yield Math.pow(i,2)
        ).foldRight(0.0)(_+_)

        (x1-x2).toInt

    def seventh =
        def primeListFirst(n: Int): List[BigInt] =
            natural.filter(isPrimeBigInt).take(n).toList

        primeListFirst(10002).last

    def eighth =
        val digits = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

        def listWindowParser(s: String, n: Int): List[String] =
            (for 
                i <- 0 to (s.size-n)
            yield (s.take(n+i)).drop(i)).toList

        listWindowParser(digits, 13)
        .map(a => (a.toList.map(e => BigInt(e.toString()))))
        .map(e => e.foldRight(BigInt(1))((a, b) => a.*(b)))
        .max

    def nineth =
        (for 
            a <- 0 to 1000
            b <- 0 to 1000
            c2 = Math.pow(a, 2) + Math.pow(b, 2)
            if isSqrt(c2)
            c = Math.sqrt(c2).toInt
            if a < b && b < c 
            if a + b + c == 1000
        yield (a, b, c)).toList.map(e => e.toList.foldRight(1)(_*_))

    def tenth =
        natural.take(2000000).filter(isPrimeBigInt).toList.foldRight(BigInt(0))(_+_)

    def eleventh =
        val table: List[List[Int]] = List(
            List(8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8),
            List(49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0),
            List(81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65),
            List(52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91),
            List(22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80),
            List(24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50),
            List(32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70),
            List(67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21),
            List(24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72),
            List(21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95),
            List(78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92),
            List(16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57),
            List(86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58),
            List(19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40),
            List(4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66),
            List(88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69),
            List(4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36),
            List(20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16),
            List(20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54),
            List(1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48)
            )

        def getUpList(x: Int, y: Int, n: Int): List[Int] =
            if n <= 0 || y-n+1 < 0 then List.empty
            else table(y)(x) :: getUpList(x, y-1, n-1) 

        def getDownList(x: Int, y: Int, n: Int): List[Int] =
            if n <= 0 || y+n > 20 then List.empty
            else table(y)(x) :: getDownList(x, y+1, n-1) 

        def getLeftList(x: Int, y: Int, n: Int): List[Int] =
            if n <= 0 || x-n+1 < 0 then List.empty
            else table(y)(x) :: getLeftList(x-1, y, n-1)

        def getRightList(x: Int, y: Int, n: Int): List[Int] =
            if n <= 0 || x+n > 20 then List.empty
            else table(y)(x) :: getRightList(x+1, y, n-1)

        def getDiagLeftList(x: Int, y: Int, n: Int): List[Int] = 
            if n <= 0 || x-n+1 < 0 || y+n > 20 then List.empty
            else table(y)(x) :: getDiagLeftList(x-1, y+1, n-1)

        def getDiagRightList(x: Int, y: Int, n: Int): List[Int] =
            if n <= 0 || x+n > 20 || y+n > 20 then List.empty
            else table(y)(x) :: getDiagRightList(x+1, y+1, n-1)

        (for 
            x <- 0 to 19
            y <- 0 to 19 

            up = getUpList(x, y, 4)
            down = getDownList(x, y, 4)
            left = getLeftList(x, y, 4)
            right = getRightList(x, y, 4)
            diagR = getDiagRightList(x, y, 4)
            diagL = getDiagLeftList(x, y, 4)

        yield (List(up, down, left, right, diagR, diagL).map(e => e.foldRight(1)(_*_))).max).max

    def twelth =
        lazy val triangle: LazyList[BigInt] = LazyList.cons(BigInt(1), from(2).zip(triangle).map(n => n._1 + n._2))

        def getFactorNumber(n: BigInt): Int =
            val s = sqrt(n) 
            @tailrec
            def helper(acc: Int, e: Int): Int =
                if acc > s then e*2 - (if isSqrt(n.toDouble) then 1 else 0)
                else if n%acc == 0 then helper(acc+1, e+1)
                else helper(acc+1, e)
            
            helper(2, 1)

        triangle.map(e => (getFactorNumber(e), e)).find(_._1 >= 500)

    def thirteenth = BigInt("37107287533902102798797998220837590246510135740250")+BigInt("46376937677490009712648124896970078050417018260538")+BigInt("74324986199524741059474233309513058123726617309629")+BigInt("91942213363574161572522430563301811072406154908250")+BigInt("23067588207539346171171980310421047513778063246676")+BigInt("89261670696623633820136378418383684178734361726757")+BigInt("28112879812849979408065481931592621691275889832738")+BigInt("44274228917432520321923589422876796487670272189318")+BigInt("47451445736001306439091167216856844588711603153276")+BigInt("70386486105843025439939619828917593665686757934951")+BigInt("62176457141856560629502157223196586755079324193331")+BigInt("64906352462741904929101432445813822663347944758178")+BigInt("92575867718337217661963751590579239728245598838407")+BigInt("58203565325359399008402633568948830189458628227828")+BigInt("80181199384826282014278194139940567587151170094390")+BigInt("35398664372827112653829987240784473053190104293586")+BigInt("86515506006295864861532075273371959191420517255829")+BigInt("71693888707715466499115593487603532921714970056938")+BigInt("54370070576826684624621495650076471787294438377604")+BigInt("53282654108756828443191190634694037855217779295145")+BigInt("36123272525000296071075082563815656710885258350721")+BigInt("45876576172410976447339110607218265236877223636045")+BigInt("17423706905851860660448207621209813287860733969412")+BigInt("81142660418086830619328460811191061556940512689692")+BigInt("51934325451728388641918047049293215058642563049483")+BigInt("62467221648435076201727918039944693004732956340691")+BigInt("15732444386908125794514089057706229429197107928209")+BigInt("55037687525678773091862540744969844508330393682126")+BigInt("18336384825330154686196124348767681297534375946515")+BigInt("80386287592878490201521685554828717201219257766954")+BigInt("78182833757993103614740356856449095527097864797581")+BigInt("16726320100436897842553539920931837441497806860984")+BigInt("48403098129077791799088218795327364475675590848030")+BigInt("87086987551392711854517078544161852424320693150332")+BigInt("59959406895756536782107074926966537676326235447210")+BigInt("69793950679652694742597709739166693763042633987085")+BigInt("41052684708299085211399427365734116182760315001271")+BigInt("65378607361501080857009149939512557028198746004375")+BigInt("35829035317434717326932123578154982629742552737307")+BigInt("94953759765105305946966067683156574377167401875275")+BigInt("88902802571733229619176668713819931811048770190271")+BigInt("25267680276078003013678680992525463401061632866526")+BigInt("36270218540497705585629946580636237993140746255962")+BigInt("24074486908231174977792365466257246923322810917141")+BigInt("91430288197103288597806669760892938638285025333403")+BigInt("34413065578016127815921815005561868836468420090470")+BigInt("23053081172816430487623791969842487255036638784583")+BigInt("11487696932154902810424020138335124462181441773470")+BigInt("63783299490636259666498587618221225225512486764533")+BigInt("67720186971698544312419572409913959008952310058822")+BigInt("95548255300263520781532296796249481641953868218774")+BigInt("76085327132285723110424803456124867697064507995236")+BigInt("37774242535411291684276865538926205024910326572967")+BigInt("23701913275725675285653248258265463092207058596522")+BigInt("29798860272258331913126375147341994889534765745501")+BigInt("18495701454879288984856827726077713721403798879715")+BigInt("38298203783031473527721580348144513491373226651381")+BigInt("34829543829199918180278916522431027392251122869539")+BigInt("40957953066405232632538044100059654939159879593635")+BigInt("29746152185502371307642255121183693803580388584903")+BigInt("41698116222072977186158236678424689157993532961922")+BigInt("62467957194401269043877107275048102390895523597457")+BigInt("23189706772547915061505504953922979530901129967519")+BigInt("86188088225875314529584099251203829009407770775672")+BigInt("11306739708304724483816533873502340845647058077308")+BigInt("82959174767140363198008187129011875491310547126581")+BigInt("97623331044818386269515456334926366572897563400500")+BigInt("42846280183517070527831839425882145521227251250327")+BigInt("55121603546981200581762165212827652751691296897789")+BigInt("32238195734329339946437501907836945765883352399886")+BigInt("75506164965184775180738168837861091527357929701337")+BigInt("62177842752192623401942399639168044983993173312731")+BigInt("32924185707147349566916674687634660915035914677504")+BigInt("99518671430235219628894890102423325116913619626622")+BigInt("73267460800591547471830798392868535206946944540724")+BigInt("76841822524674417161514036427982273348055556214818")+BigInt("97142617910342598647204516893989422179826088076852")+BigInt("87783646182799346313767754307809363333018982642090")+BigInt("10848802521674670883215120185883543223812876952786")+BigInt("71329612474782464538636993009049310363619763878039")+BigInt("62184073572399794223406235393808339651327408011116")+BigInt("66627891981488087797941876876144230030984490851411")+BigInt("60661826293682836764744779239180335110989069790714")+BigInt("85786944089552990653640447425576083659976645795096")+BigInt("66024396409905389607120198219976047599490197230297")+BigInt("64913982680032973156037120041377903785566085089252")+BigInt("16730939319872750275468906903707539413042652315011")+BigInt("94809377245048795150954100921645863754710598436791")+BigInt("78639167021187492431995700641917969777599028300699")+BigInt("15368713711936614952811305876380278410754449733078")+BigInt("40789923115535562561142322423255033685442488917353")+BigInt("44889911501440648020369068063960672322193204149535")+BigInt("41503128880339536053299340368006977710650566631954")+BigInt("81234880673210146739058568557934581403627822703280")+BigInt("82616570773948327592232845941706525094512325230608")+BigInt("22918802058777319719839450180888072429661980811197")+BigInt("77158542502016545090413245809786882778948721859617")+BigInt("72107838435069186155435662884062257473692284509516")+BigInt("20849603980134001723930671666823555245252804609722")+BigInt("53503534226472524250874054075591789781264330331690")

    def fourteenth =
        def iterative(n: BigInt): List[BigInt] =
            @tailrec
            def helper(acc: List[BigInt]): List[BigInt] =
                val h = acc.head
                if h == BigInt(1) then acc
                else if h%2==0 then helper((h/2)::acc)
                else helper((3*h+1)::acc)

            helper(List(n))

        natural.take(1000000).map(e => (iterative(e).size, e)).max

    def fifteenth = 
        def moveDownRightTailRec(x: Int, y: Int, l: Int): BigInt =
            @tailrec
            def helper(pos: List[(Int, Int)], acc: BigInt): BigInt =
                pos match
                    case Nil => acc
                    case head :: tail => 
                        if head._1 == l && head._2 == l then helper(tail, acc+1)
                        // else if head._1 == head._2 then helper((head._1+1, head._2), 0)*2
                        else if head._1+1 > l && !(head._2+1 > l) then helper((head._1, head._2+1)::tail, acc)
                        else if !(head._1+1 > l) && head._2+1 > l then helper((head._1+1, head._2)::tail, acc)
                        else helper((head._1, head._2+1)::(head._1+1, head._2)::tail, acc)
                
            helper(List((x, y)), 0)

        def moveDownRightMemoization(x: Int, y: Int, l: Int): BigInt = 
            val cache: Map[Int, BigInt] = Map()
            def helper(x: Int, y: Int, acc: BigInt): BigInt =
                if x == l && y == l then acc+1
                else if x == y then cache.getOrElseUpdate(x, 2*helper(x+1, y, 0))
                else if x+1 > l && !(y+1 > l) then helper(x, y+1, acc)
                else if !(x+1 > l) && y+1 > l then helper(x+1, y, acc)
                else helper(x+1, y, acc) + helper(x, y+1, acc)

            helper(x, y, 0)

        moveDownRightMemoization(0, 0, 20)

    def sixteenth = 
        pow(2, 1000).toString().toList.foldRight(0)((a, b) => a.toString().toInt+b.toString().toInt)

    def seventeenth =
        def charDigitToString(c: Char): String =
            c match
                case '0' => ""
                case '1' => "one"
                case '2' => "two"
                case '3' => "three"
                case '4' => "four"
                case '5' => "five"
                case '6' => "six"
                case '7' => "seven"
                case '8' => "eight"
                case '9' => "nine"
                case _ => "ERROR"

        def numberStringToString(number: String): String = 
            number.size match
                case 4 => 
                    if charDigitToString(number.head) == "" then
                        numberStringToString(number.tail) 
                    else if charDigitToString(number.tail.head) == "" && charDigitToString(number.tail.tail.head) != "" && charDigitToString(number.tail.tail.tail.head) != "" then 
                        charDigitToString(number.head) + " thousand and " + numberStringToString(number.tail) 
                    else 
                        charDigitToString(number.head) + " thousand" + numberStringToString(number.tail)
                case 3 => 
                    if charDigitToString(number.head) == "" then //0XX
                        numberStringToString(number.tail) 
                    else if charDigitToString(number.tail.head) == "" && charDigitToString(number.tail.tail.head) == "" then
                        charDigitToString(number.head) + " hundred" + numberStringToString(number.tail)
                    else 
                        charDigitToString(number.head) + " hundred and " + numberStringToString(number.tail) 
                case 2 => charDigitToString(number.head) match
                    case "" => numberStringToString(number.tail)
                    case "one" => charDigitToString(number.tail.head) match
                        case "" => "ten"
                        case "one" => "eleven"
                        case "two" => "twelve"
                        case "three" => "thirteen"
                        case "four" => "fourteen"
                        case "five" => "fifteen"
                        case "six" => "sixteen"
                        case "seven" => "seventeen"
                        case "eight" => "eighteen"
                        case "nine" => "nineteen"
                    case "two" => if charDigitToString(number.tail.head) == "" then "twenty" else "twenty-" + numberStringToString(number.tail)
                    case "three" => if charDigitToString(number.tail.head) == "" then "thirty" else "thirty-" + numberStringToString(number.tail)
                    case "four" => if charDigitToString(number.tail.head) == "" then "forty" else "forty-" + numberStringToString(number.tail)
                    case "five" => if charDigitToString(number.tail.head) == "" then "fifty" else "fifty-" + numberStringToString(number.tail)
                    case "six" => if charDigitToString(number.tail.head) == "" then "sixty" else "sixty-" + numberStringToString(number.tail)
                    case "seven" => if charDigitToString(number.tail.head) == "" then "seventy" else "seventy-" + numberStringToString(number.tail)
                    case "eight" => if charDigitToString(number.tail.head) == "" then "eighty" else "eighty-" + numberStringToString(number.tail)
                    case "nine" => if charDigitToString(number.tail.head) == "" then "ninety" else "ninety-" + numberStringToString(number.tail)

                case 1 => charDigitToString(number.head)
            
        def countNumberStringLength(n: Int): Int =
            numberStringToString(n.toString()).map(e => !e.isSpaceChar && !(e == '-')).count(e => e==true)

        numberStringToString("1000")

        (for 
            i <- 1 to 1000
        yield countNumberStringLength(i)).foldRight(0)(_+_)


    def eigtheenth =
        enum Tree[T]:
            case Branch(value: T , left: Tree[T], right: Tree[T]) //children: List[Tree[T]] = Nil)
            case Leaf(value : T)    

            def map[B](f: T => B): Tree[B] =
                this match
                    case Tree.Branch(v, l, r) => Tree.Branch(f(v), l.map(f), r.map(f))
                    case Tree.Leaf(v) => Tree.Leaf(f(v))

            def getValue: T =
                this match
                    case Branch(value, _, _) => value
                    case Leaf(value) => value

            def getOnlyLeaf: List[T] =
                this match
                    case Tree.Branch(value, left, right) => left.getOnlyLeaf ++ right.getOnlyLeaf
                    case Tree.Leaf(value) => value :: Nil
            
            def add(v: T)(using Numeric[T]): Tree[T] = 
                this match 
                    case Branch(value, left, right) => Branch(implicitly[Numeric[T]].plus(v, value), left, right)
                    case Leaf(value) => Leaf(implicitly[Numeric[T]].plus(v, value))
                
        def treeAdder(self: Tree[Int]): Tree[Int] =
            self match
                case Tree.Branch(value, left, right) => Tree.Branch(value, treeAdder(left.add(value)), treeAdder(right.add(value)))
                case Tree.Leaf(value) => Tree.Leaf(value)

        val tree = """75
        95 64
        17 47 82
        18 35 87 10
        20 04 82 47 65
        19 01 23 75 03 34
        88 02 77 73 07 63 67
        99 65 04 28 06 16 70 92
        41 41 26 56 83 40 80 70 33
        41 48 72 33 47 32 37 16 94 29
        53 71 44 65 25 43 91 52 97 51 14
        70 11 33 28 77 73 17 78 39 68 17 57
        91 71 52 38 17 14 91 43 58 50 27 29 48
        63 66 04 68 89 53 67 30 73 16 69 87 40 31
        04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
        """

        def createTreeFromString(s: String): Tree[String] =
            def helper(treeList: List[String], n: Int): Tree[String] =
                if treeList.tail.tail.isEmpty then Tree.Leaf(treeList.head.charAt(n*2).toString() + treeList.head.charAt(n*2+1))
                else Tree.Branch(treeList.head.charAt(n*2).toString() + treeList.head.charAt(n*2+1), helper(treeList.tail, n), helper(treeList.tail, n+1))

            def stringWithoutSpacesToList(s: String): List[String] =
                s.split("\n").toList.map(e => e.filterNot(_.isWhitespace))

            helper(stringWithoutSpacesToList(s), 0)

        treeAdder(createTreeFromString(tree).map(_.toInt)).getOnlyLeaf.max


    def nineteenth =
        enum Month:
            case January(year: Int)
            case February(year: Int)
            case March(year: Int)
            case April(year: Int)
            case May(year: Int)
            case June(year: Int)
            case July(year: Int)
            case August(year: Int)
            case September(year: Int)
            case October(year: Int)
            case November(year: Int)
            case December(year: Int)

            def leap(y: Int): Boolean = if y%4==0 && !(y%100==0) then true else if y%400==0 then true else false

            def getNumberOfDays: Int = this match
                case January(year) => 31
                case February(year) => if leap(year) then 29 else 28
                case March(year) => 31
                case April(year) => 30
                case May(year) => 31
                case June(year) => 30
                case July(year) => 31
                case August(year) => 31
                case September(year) => 30
                case October(year) => 31
                case November(year) => 30
                case December(year) => 31

            def getYear: Int = this match
                case January(year) => year
                case February(year) => year
                case March(year) => year
                case April(year) => year
                case May(year) => year
                case June(year) => year
                case July(year) => year
                case August(year) => year
                case September(year) => year
                case October(year) => year
                case November(year) => year
                case December(year) => year

            def addOneMonth: Month = this match
                case January(year) => February(year)
                case February(year) => March(year)
                case March(year) => April(year)
                case April(year) => May(year)
                case May(year) => June(year)
                case June(year) => July(year)
                case July(year) => August(year)
                case August(year) => September(year)
                case September(year) => October(year)
                case October(year) => November(year)
                case November(year) => December(year)
                case December(year) => January(year+1)
            
        enum Day:
            case Sunday(day: Int, month: Month)
            case Monday(day: Int, month: Month)
            case Tuesday(day: Int, month: Month)
            case Wednesday(day: Int, month: Month)
            case Thursday(day: Int, month: Month)
            case Friday(day: Int, month: Month)
            case Saturday(day: Int, month: Month)

            def getDay: Int = this match
                case Sunday(day, month) => day
                case Monday(day, month) => day
                case Tuesday(day, month) => day
                case Wednesday(day, month) => day
                case Thursday(day, month) => day
                case Friday(day, month) => day
                case Saturday(day, month) => day

            def getMonth: Month = this match
                case Sunday(day, month) => month
                case Monday(day, month) => month
                case Tuesday(day, month) => month
                case Wednesday(day, month) => month
                case Thursday(day, month) => month
                case Friday(day, month) => month
                case Saturday(day, month) => month

            def isSunday: Boolean = this match
                case Sunday(day, month) => true
                case _ => false

            def addOneDay: Day = this match
                case Sunday(day, month) => if day == month.getNumberOfDays then Monday(1, month.addOneMonth) else Monday(day+1, month)
                case Monday(day, month) => if day == month.getNumberOfDays then Tuesday(1, month.addOneMonth) else Tuesday(day+1, month)
                case Tuesday(day, month) => if day == month.getNumberOfDays then Wednesday(1, month.addOneMonth) else Wednesday(day+1, month)
                case Wednesday(day, month) => if day == month.getNumberOfDays then Thursday(1, month.addOneMonth) else Thursday(day+1, month)
                case Thursday(day, month) => if day == month.getNumberOfDays then Friday(1, month.addOneMonth) else Friday(day+1, month)
                case Friday(day, month) => if day == month.getNumberOfDays then Saturday(1, month.addOneMonth) else Saturday(day+1, month)
                case Saturday(day, month) => if day == month.getNumberOfDays then Sunday(1, month.addOneMonth) else Sunday(day+1, month)

        def whatDayIs(day: Int, month: Month): Day =
            @tailrec
            def helper(acc: Day): Day =
                if acc.getDay == day && acc.getMonth == month then acc
                else helper(acc.addOneDay)
            
            helper(Day.Monday(1, Month.January(1900)))

        def countSundays(start: Day, end: Day): Int =
            def helper(s: Day, acc: Int): Int = 
                if s == end then acc
                else if s.isSunday && s.getDay == 1 then helper(s.addOneDay, acc+1) else helper(s.addOneDay, acc)

            helper(start, 0)

        countSundays(whatDayIs(1, Month.January(1901)), whatDayIs(1, Month.January(2001)))
            
    def twenteeth =
        factorial(100).toString().toList.map(c => c.toString().toInt).foldRight(BigInt(0))(_+_)

    def twentyfirst = 
        lazy val amicable_numbers: LazyList[BigInt] = d.filter((e, i) => i!=e && d(e.toInt-1)._1 == i).map(_._1)

        amicable_numbers.take(10).foldRight(BigInt(0))(_+_)

    def twentysecond = 
        val fileContents = Source.fromFile("src/main/resources/0022_names.txt").getLines.mkString

        fileContents.split(",").toList.map(s => s.filterNot(_ == '\"')).sorted.zipWithIndex.map((s, i) => s.toCharArray().map(_.toInt-64).foldRight(0)(_+_)*(i+1)).foldRight(BigInt(0))(_+_)

    def twentythird = 
        lazy val perfect_numbers: LazyList[BigInt] = d.filter((e, i) => e==i).map(_._2)
        lazy val deficient_numbers: LazyList[BigInt] = d.filter((e, i) => e<i).map(_._2)
        lazy val abundant_numbers: LazyList[BigInt] = d.filter((e, i) => e>i).map(_._2)

        def canBeWrittenAsSumOfTwoAbundantMemoizaion(n: BigInt): Boolean =
            val cache: Map[BigInt, Boolean] = Map()
            val abundant_list = Source.fromFile("src/main/resources/abundant_list.txt").getLines.mkString.split(", ").map(BigInt(_)).toList
            
            @tailrec
            def helper(acc: Int): Boolean =
                val value = abundant_list(acc)
                val complement = n - value

                if value > n then false
                else if abundant_list.contains(complement) then true
                else helper(acc+1)
            
            if n > 28123 then true 
            else cache.getOrElseUpdate(n, helper(0))

        natural.take(28123).toList.filterNot(canBeWrittenAsSumOfTwoAbundantMemoizaion(_)).foldRight(BigInt(0))(_+_)

    def twentyfourth = 
        List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).permutations.toList(999999)

    def twentyfifth = 
        lazy val fibo: LazyList[BigInt] = LazyList.cons(1, LazyList.cons(1, fibo.zip(fibo.tail).map(n => n._1 + n._2)))

        fibo.zipWithIndex.map((e, i) => (e, i+1)).find((e, i) => e.toString().size>=1000)

    def fourtyeight =
        var t = BigInt(0)
        for i <- 1 to 1000 do
            t+=pow(BigInt(i), i)

        t.toString().takeRight(10)

    def thirty = 
        def sumOfNthPow(n: Int): LazyList[BigInt] = natural.map(e => e.toString().toList.map(_.toString().toInt).foldRight(BigInt(0))((a, b) => pow(a, n) + b))

        sumOfNthPow(5).take(1000000).toList.zipWithIndex.filter((e, i) => e==i+1).map(_._1).foldRight(BigInt(0))(_+_)-1

    def twentyeight =
        def sumDiagSpiral(n: Int): Int =
            if n == 1 then 1 
            else n*n + n*(n-1)+1 + n*(n-2)+2 + n*(n-3)+3 + sumDiagSpiral(n-2)

        sumDiagSpiral(1001)

    def twentynine = 
        def removeDuplicatesOffSortedList[T](l: List[T]): List[T] =
            l match
                case head :: Nil => head :: Nil
                case head :: next => if head==next.head then removeDuplicatesOffSortedList(next) else head :: removeDuplicatesOffSortedList(next)

        removeDuplicatesOffSortedList(
            (for 
                a <- 2 to 100
                b <- 2 to 100
            yield pow(a, b)).sorted.toList
        ).size

    def thirtyfour = 
        lazy val sumOfFactDigits: LazyList[BigInt] = natural.map(e => e.toString().toList.map(_.toString().toInt).foldRight(BigInt(0))((a, b) => factorial(a)+b))
        sumOfFactDigits.take(1000000).zipWithIndex.filter((e, i) => e == i+1).map(_._1).toList

    def thirtysix = 
        def base10To2(n: BigInt): String =
            def helper(n: BigInt, acc: String): String = 
                if n == 1 then "1" ++ acc
                else helper(n/2, (n%2).toString() ++ acc)

            helper(n, "")

        natural.take(999999).filter(n => isPalindrome(base10To2(n)) && isPalindrome(n.toString())).toList.foldRight(BigInt(0))(_+_)

    def twentyseven = 
        def quadraticPrime(a: Int, b: Int): List[BigInt] = from(0).take(200).map(n => n*n+n*a+b).toList

        def longestChainPrimes(list: List[BigInt]): Int = list.zipWithIndex.find((n, i) => !isPrimeBigInt(n)).get._2

        (for 
            a <- -999 to 999
            b <- -1000 to 1000
        yield (longestChainPrimes(quadraticPrime(a, b)), a, b)).max

    def thirtyone = 
        def coinSums(n : Int): Int =
            def helper(rest: Int, prev: Int): Int =
                if rest >= 200 then 
                    if prev == 200 then helper(rest-200, 200) + helper(rest-100, 100) + helper(rest-50, 50) + helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 100 then helper(rest-100, 100) + helper(rest-50, 50) + helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 50 then helper(rest-50, 50) + helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 20 then helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 10 then helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 5 then helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 100 then 
                    if prev >= 100 then helper(rest-100, 100) + helper(rest-50, 50) + helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 50 then helper(rest-50, 50) + helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 20 then helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 10 then helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 5 then helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 50 then 
                    if prev >= 50 then helper(rest-50, 50) + helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 20 then helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 10 then helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 5 then helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 20 then
                    if prev >= 20 then helper(rest-20, 20) + helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 10 then helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 5 then helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 10 then 
                    if prev >= 10 then helper(rest-10, 10) + helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 5 then helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 5 then
                    if prev >= 5 then helper(rest-5, 5) + helper(rest-2, 2) + helper(rest-1, 1)
                    else if prev == 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 2 then 
                    if prev >= 2 then helper(rest-2, 2) + helper(rest-1, 1)
                    else helper(rest-1, 1)
                else if rest >= 1 then 
                    helper(rest-1, 1)
                else 1

            helper(n, 200)


        coinSums(200)

    def thirtyfive = 
        def isCircularPrime(n: BigInt): Boolean = 
            !circularPerm(n.toString()).map(n => isPrimeBigInt(n.toInt)).contains(false)

        def circularPerm(s: String): List[String] =
            def helper(s: String, acc: Int): List[String] =
                if acc == s.size then Nil
                else
                    val newS = s.tail + s.head 
                    newS :: helper(newS, acc+1)

            helper(s, 0)

        natural.take(1000000).map(isCircularPrime(_)).toList.count(_==true)

    










