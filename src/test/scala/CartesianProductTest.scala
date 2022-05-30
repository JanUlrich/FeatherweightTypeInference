import hb.dhbw.CartesianProduct
import org.scalatest.FunSuite

class CartesianProductTest extends FunSuite{

  test("nextProduct"){
    val test = new CartesianProduct[Int](Set(Set(1,2),Set(4,3)))
    val result = List(1,2,3,4).map( _ => test.nextProduct())
    assert(result.contains(Set(1,4)))
  }
}
