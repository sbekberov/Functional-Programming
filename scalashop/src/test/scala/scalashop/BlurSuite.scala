package scalashop

import org.junit._
import org.junit.Assert.{assertEquals, assertNotEquals}

import scala.util.Random

class BlurSuite {
  def generateImg(images: List[Img]): Unit = {
    for (src <- images; row <- 0 until src.height; col <- 0 until src.width) {
      src(col, row) = row * src.width + col
    }
  }

  /**
   * HorizontalBoxBlur parBlur should not forget the last strip
   */
  @Test def HorizontalLastStrip(): Unit = {
    val src = new Img(3, 4)
    val dst = new Img(3, 4)
    generateImg(List(src, dst))
    HorizontalBoxBlur.parBlur(src, dst, 2, 1)

    var result: List[Boolean] = List()
    val maxRow = (src.height - src.height / 2) until src.height
    for (rowNum <- maxRow; columnNum <- 0 until src.width) {
      result = result :+ (dst.apply(columnNum, rowNum) == src.apply
      (columnNum, rowNum))
      assertNotEquals("blurred image must be distinct from the original", true, result.forall(_ == true))
    }
  }

  @Test def VerticalLastStrip(): Unit = {
    val src = new Img(4, 3)
    val dst = new Img(4, 3)
    generateImg(List(src, dst))
    VerticalBoxBlur.parBlur(src, dst, 2, 1)

    var result: List[Boolean] = List()
    val maxCol = (src.width - src.width / 2) until src.width
    for ( columnNum <- maxCol; rowNum <- 0 until src.height)  {
      result = result :+ (dst.apply(columnNum, rowNum) == src.apply
      (columnNum, rowNum))
    }
    assertNotEquals("blured image must be distinct from the original", true, result.forall(_ == true))
  }

  /**
   * boxBlurKernel should return the correct value
   * on an interior pixel of a 3x4 image with radius 1
   * (використайте зручну вам кількість пікселів та радіус)
   */
  @Test def BoxBlurKernel3x4Img(): Unit = {
    val src = new Img(3, 4)
    generateImg(List(src))
    assertEquals(4,  boxBlurKernel(src, 1, 1, 1))
  }

  /**
   * boxBlurKernel should correctly handle radius 0
   */
  @Test def BoxBlurKernelZeroRadius(): Unit = {
    val src = new Img(5, 5)
    generateImg(List(src))
    assertEquals(src.apply(2, 2), boxBlurKernel(src, 2, 2, 0))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}