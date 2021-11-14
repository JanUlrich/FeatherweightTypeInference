package hb.dhbw

class CartesianProduct[A](private val setOfSets: List[List[A]]){
  def productWith(product: CartesianProduct[A]) = {
    val ret = new CartesianProduct[A](setOfSets ++ product.setOfSets)
    var base: Long = 1
    ret.sizes = ret.setOfSets.map(_.size)
    ret.sizes.foreach(size => {
      base = base * size
    })
    ret.max = base
    ret.i = i
    ret
  }

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
