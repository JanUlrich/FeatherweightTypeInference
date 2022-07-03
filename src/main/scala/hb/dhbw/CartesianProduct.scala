package hb.dhbw

class CartesianProductBuilder[A](){
  var ret : Set[Set[A]] = Set()
  def build() : CartesianProduct[A] = new CartesianProduct[A](ret)

  def addSingleton(a:A){
    ret += Set(a)
  }
  def add(as : Set[A]): Unit ={
    ret += as
  }
}

class CartesianProduct[A](private val setOfSets: List[List[A]]){
  private var sizes: List[Int] = null
  private var max: Long = 1
  private var i: Long = 0

  def this(setOfSets: Set[Set[A]]) {
    this(setOfSets.map(_.toList).toList)
    var base: Long = 1
    sizes = this.setOfSets.map(_.size)
    if(sizes.size == 0) base = 0
    sizes.foreach(size => {
      base = base * size
    })
    max = base
  }

  def hasNext() = i < max

  def nextProduct() = {
    var currentI = i
    val ret = setOfSets.map(set => {
      val num = (currentI%set.size)
      currentI = currentI/set.size
      set(num.toInt)
    })
    i = i + 1
    ret.toSet
  }
}
