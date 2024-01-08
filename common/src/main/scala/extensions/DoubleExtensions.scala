package extensions

import java.math.BigDecimal as JavaBD

object DoubleExtensions {


  extension (value: Double) {
    def doubleToJavaBD: JavaBD = JavaBD.valueOf(value)

  }


}