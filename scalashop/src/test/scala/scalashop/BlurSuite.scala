package scalashop

import org.junit._
import org.mockito.{ArgumentMatchers, Mockito}

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test def `testSimpleBoxBlure`: Unit = {
    val i: Img = new Img(3, 3, List(2, 2, 2, 2, 1, 2, 2, 2, 2).toArray)
    val r = boxBlurKernel(i, 1, 1, 1)
    val e: RGBA = 1
    assert(e == r, "expected 1 but was " + r)

    val i2: Img = new Img(3, 3, List(2, 2, 2, 2, 1, 4, 4, 4, 4).toArray)
    val r2 = boxBlurKernel(i2, 1, 1, 1)
    assert(2 == r2, "expected 2 but was " + r2)
  }

  @Test def `parallel filter test`: Unit = {
    val src: Img = new Img(4, 2, List(2, 2, 2, 2, 2, 2, 2, 2).toArray)
    val dst: Img = Mockito.spy(new Img(4, 2, List(1, 1, 1, 1, 1, 1, 1, 1).toArray))

    HorizontalBoxBlur.parBlur(src, dst, 2, 1)

    val inOrder = Mockito.inOrder(dst)

    inOrder.verify(dst).update(ArgumentMatchers.eq(0), ArgumentMatchers.eq(0), ArgumentMatchers.any())
    inOrder.verify(dst).update(ArgumentMatchers.eq(1), ArgumentMatchers.eq(1), ArgumentMatchers.any())
    inOrder.verify(dst).update(ArgumentMatchers.eq(2), ArgumentMatchers.eq(2), ArgumentMatchers.any())
    inOrder.verify(dst).update(ArgumentMatchers.eq(3), ArgumentMatchers.eq(3), ArgumentMatchers.any())
    /*inOrder.verify(dst).update(ArgumentMatchers.eq(4), ArgumentMatchers.eq(4), ArgumentMatchers.any())
    inOrder.verify(dst).update(ArgumentMatchers.eq(5), ArgumentMatchers.eq(5), ArgumentMatchers.any())
    inOrder.verify(dst).update(ArgumentMatchers.eq(6), ArgumentMatchers.eq(6), ArgumentMatchers.any())
    inOrder.verify(dst).update(ArgumentMatchers.eq(7), ArgumentMatchers.eq(7), ArgumentMatchers.any())*/
  }
}
