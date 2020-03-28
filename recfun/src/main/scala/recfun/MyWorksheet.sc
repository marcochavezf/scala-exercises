// val xyz: Int = 125
// println("Hello, worksheets!")
// 456 + xyz

// val radius = 5
// radius + 1

// def fn(p: Int) = p + 1
// fn(radius)

// "12323".toList

// def balance(chars: List[Char]): Boolean = {

//   def countBalance(count: Int, chars: List[Char]): Boolean = 
//     if (count < 0) {
//       false
//     } else if (chars.isEmpty) {
//       count == 0
//     } else {
//       val delta = if (chars.head == '(') {
//         1
//       } else if (chars.head == ')') {
//         -1
//       } else {
//         0
//       }
//       countBalance(count + delta, chars.tail)
//     }

//   countBalance(0, chars)
// }

// balance("(if (zero? x) max (/ 1 x))".toList)
// balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
// balance(":-)".toList)
// balance("())(".toList)

def countChange(money: Int, coins: List[Int]): Int = {
  if (money < 0 || coins.isEmpty) {
    0
  } else if (money == 0) {
    1
  } else {
    countChange(money - coins.head, coins) + 
    countChange(money, coins.tail)
  }
}

countChange(4, List(1,2))
countChange(4, List(2,1))
countChange(4, List(1,2,3))
countChange(4, List(3,2,1))
countChange(4, List(1,3))