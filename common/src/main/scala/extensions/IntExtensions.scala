package extensions

import java.math.BigDecimal as JavaBD

object IntExtensions {


  extension (value: Int) {
    def intToJavaBD: JavaBD = JavaBD.valueOf(value)

  }


}