import hb.dhbw.CartesianProduct
import org.scalatest.FunSuite

class CartesianProductTest extends FunSuite{

  test("nextProduct"){
    val test = new CartesianProduct[Int](Set(Set(1,2),Set(4,3)))
    val result = List(1,2,3,4).map( _ => test.nextProduct())
    assert(result.contains(Set(1,4)))
  }

  test("productWith"){
    val test = new CartesianProduct[Int](Set(Set(1,2),Set(4,3)))
    val test2 = new CartesianProduct[Int](Set(Set(5,6), Set(7,8)))
    val test3 = test.productWith(test2)
    val result = for( i <- 1 to 16) yield test3.nextProduct()
    assert(result.contains(Set(2,3,6,8)))
    assert(result.toSet.size == 16)
    println(result)
  }

}
