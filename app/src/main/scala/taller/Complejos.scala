package taller

/**
 * Punto 4. Un número complejo r + i·i, con r la parte real e i la imaginaria. Los objetos no
 * cambian: cada operación devuelve uno nuevo.
 *
 * Tal como está compila y las pruebas quedan en rojo.
 */
class Complejos(val r: Double, val i: Double) {

  val a = redondeo(r)
  val b = redondeo(i)

  def redondeo(n: Double): Double =
    Math.round(n * 1000) / 1000.0

  def +(otro: Complejos): Complejos = {
    val r = this.r + otro.r
    val i = this.i + otro.i
    new Complejos(r, i)
  }

  def -(otro: Complejos): Complejos = {
    val r = this.r - otro.r
    val i = this.i - otro.i
    new Complejos(r, i)
  }
  // a1·a2 - b1·b2
  // a1·b2 + a2·b1
  def *(otro: Complejos): Complejos = {
    val r = this.r * otro.r - this.i * otro.i
    val i = this.r * otro.i + otro.r * this.i
    new Complejos(r, i)
  }
  // (a1·a2 + b1·b2) / (a2² + b2²)
  // (a2·b1 - a1·b2) / (a2² + b2²)
  def /(otro: Complejos): Complejos = {
    val r = (this.r * otro.r + this.i * otro.i) / (otro.r * otro.r + otro.i * otro.i)
    val i = (otro.r * this.i - this.r * otro.i) / (otro.r * otro.r + otro.i * otro.i)
    new Complejos(r, i)
  }

  // "a + bi" con las dos partes redondeadas a tres decimales; si la parte
  // imaginaria es negativa, "a - bi".
  override def toString: String =
    if (this.b >= 0) {
      this.a + " + " + this.b + "i"
    } else {
      this.a + " - " + (-this.b) + "i"
    }
}
