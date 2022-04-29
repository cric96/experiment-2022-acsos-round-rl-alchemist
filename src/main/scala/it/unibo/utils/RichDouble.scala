package it.unibo.utils

object RichDouble {
  /** Add functionality for the double type.
    * @param d
    *   the data wrapped
    */
  implicit class TypeEnrichment(val d: Double) extends AnyVal {
    def default = 0.00001
    def ~=(d2: Double): Boolean = (d - d2).abs < default
  }
}
