class Line(ps:Array[Point]) {

  val xs = new Array[Double](ps.length)
  val ys = new Array[Double](ps.length)
  for(i <- 0 until ps.length){
    xs(i)=ps(i).x
    ys(i)=ps(i).y
  }
  val a=Util.cov(xs,ys) / Util.variance(xs)
  val b=ys.sum/ys.length - a *xs.sum/xs.length

  val mostFarPoint = {
    var maxDist = 0.0
    ps.foreach(point=>{
      val value = dist(point)
      maxDist = Math.max(value,maxDist)
    })
    maxDist
  }

  def f(x:Double):Double =  a*x+b

  def dist(p:Point):Double = Math.abs(f(p.x)-p.y)

  override def toString: String = {
    super.toString
    return s"${a},${b},${mostFarPoint}"
  }
}
