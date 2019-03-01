object R5 extends App {

  //@annotation.tailrec  // uncomment this and try compiling
  def addInterval(x: Int, y: Int): Int = {
    if (x == y) x else x + addInterval(x + 1, y)
  }

  println(addInterval(5, 10))
  // println(addInterval(1, 7867))  // stackoverflow, uncomment to try out

  def addIntervalBetter(x: Int, y: Int): Int = {
    @annotation.tailrec
    def addIntervalTR(x: Int, y: Int, acc: Int): Int = {
      if (x == y) x + acc else addIntervalTR(x + 1, y, x + acc)
    }
  addIntervalTR(x, y, 0)
  }

  println(addIntervalBetter(5, 10))
  println(addIntervalBetter(1, 7867))

  println(addIntervalBetter(1, 5000) == addInterval(1, 5000)) 
}