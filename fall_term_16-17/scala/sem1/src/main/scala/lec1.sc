var i = 1
var fact = 1
while (i < 10) {
    fact *= i
    i += 1
}
fact

val a: Any = "text"
a match {
    case 1 | 2 => 1
    case x: Int => 2
    case "text" => 3
}

def dividesBy3Or5(n: Int): Long = {
    (1 to n).filter(p => p % 3 == 0 || p % 5 == 0).sum
}

dividesBy3Or5(10)

def fact(n: Int): BigInt = {
    var res: BigInt = 1
    var i = 1
    while (i <= n) {
        res *= i
        i += 1
    }
    res
}

(1 to 10).fold(1)((a, b) => a * b<ee)