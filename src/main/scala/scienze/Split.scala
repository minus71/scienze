package scienze

object Split {
  def main(args: Array[String]): Unit = {
    val xs0 = Seq(1, 1)

    val MAX = 21;

    val result = (1 to MAX).foldLeft(Seq(xs0))((xs, _) => {
      next(xs.head) +: xs
    }).reverse

    // result.foreach(println)

    println(result(5))
    
    println(result(17).count(_==2017))
    
    //println(result.map(xs => xs.count(x => x == 12)) zip (1 to MAX))
    println("""|===============
               |   COUNT      
               |===============""".stripMargin)

    (1 to MAX) zip (
      (1 to MAX) map { k => result.map(xs => xs.count(x => x == k)) } map (_.max)) foreach { println }

    
    
    
    val generations = (1 to 22) zip result

    println("""|===============
               |   APPEAR      
               |===============""".stripMargin)
    val app = (1 to MAX + 2020) map {
      n =>
        generations find {
          case (i, xs) => xs.contains(n)
        } map {
          case (w, ss) => (n, w)
        }
    } map { case Some((x,y))=> (x,y)} 

    app foreach (println)
    println("""|===============
               |   JUMPS
               |===============""".stripMargin)
    
    (app drop(1) zip app.dropRight(1) ) map {case((il,l),(ir,r))=> (il,l-r) } foreach (println)
    
  }

  def next(xs: Seq[Int]): Seq[Int] = {
    val ys = xs zip xs.drop(1)
    xs.head +: ys.flatMap { case (l, r) => List(l + r, r) }
  }
}