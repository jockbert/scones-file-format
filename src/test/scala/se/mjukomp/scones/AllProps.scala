
package se.mjukomp.scones

import org.scalacheck.Properties

object AllProps extends Properties("se.mjukomp") {
  include(ReaderProps)
  include(WriterProps)
  include(WriteReadRoundtripProps)
}
