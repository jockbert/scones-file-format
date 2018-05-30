package se.mjukomp.scones

import org.scalacheck.Properties

object ReaderProps extends Properties("Reader") {

  property("Empty data") =
    true
  /* assertBox(w = 0, h = 0, out = "") { text() }

  def assertBox(w: Int, h: Int, out: String)(actual: Box) =
    all(
      "width" |: (actual.width ?= w),
      "height" |: (actual.height ?= h),
      "output" |: (actual.output ?= out))
      */
}
