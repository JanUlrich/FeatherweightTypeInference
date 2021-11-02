package hb.dhbw

class CartesianProduct[A](private val setOfSets: List[List[A]]){
  def productWith(product: CartesianProduct[A]) = {
    val ret = new CartesianProduct[A](setOfSets ++ product.setOfSets)
    var base: Long = 1
    ret.sizes = ret.setOfSets.map(_.size)
    ret.sizes.foreach(size => {
      ret.bases = ret.bases :+ size
      base = base * size
    })
    ret.max = base
    ret.i = i
    ret
  }

  private var sizes: List[Int] = null
  private var bases: List[Long] = List()
  private var max: Long = 1
  private var i: Long = 0

  def this(setOfSets: Set[Set[A]]) {
    this(setOfSets.map(_.toList).toList)
    var base: Long = 1
    sizes = this.setOfSets.map(_.size)
    sizes.foreach(size => {
      bases = bases :+ size
      base = base * size
    })
    max = base
  }

  def hasNext() = i < max

  def nextProduct() = {
    val baseIt = bases.iterator
    val ret = setOfSets.map(set => {
      val num = (i/baseIt.next())%set.size
      set(num.toInt)
    })
    i = i + 1
    ret.toSet
  }
}
