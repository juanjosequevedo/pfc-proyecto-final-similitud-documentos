package proyecto

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import Similitud._

@RunWith(classOf[JUnitRunner])
class PruebasPropiasSimilitud extends AnyFunSuite {

  // normalizar ----------------------------------------------------------------

  test("normalizar: cadena vacía da vector vacío") {
    assert(normalizar("") == Vector())
  }

  test("normalizar: quita comas y signos de exclamación") {
    assert(normalizar("Buenos dias, amigo!") == Vector("buenos", "dias", "amigo"))
  }

  test("normalizar: separa palabras con guiones y puntos") {
    assert(normalizar("casa-grande.verde") == Vector("casa", "grande", "verde"))
  }

  test("normalizar: solo signos de puntuación da vacío") {
    assert(normalizar("... ??? !!!") == Vector())
  }

  test("normalizar: mantiene los números dentro de la frase") {
    assert(normalizar("Hoy compre 3 manzanas y 2 peras") ==
      Vector("hoy", "compre", "3", "manzanas", "y", "2", "peras"))
  }

  // fragmentos ----------------------------------------------------------------

  test("fragmentos: vector vacío da vacío") {
    assert(fragmentos(Vector(), 2) == Vector())
  }

  test("fragmentos: si hay menos palabras que k da vacío") {
    assert(fragmentos(Vector("el", "perro"), 3) == Vector())
  }

  test("fragmentos: con k cero da vacío") {
    assert(fragmentos(Vector("el", "perro", "corre"), 0) == Vector())
  }

  test("fragmentos: con k igual al total da un solo fragmento") {
    val palabras = Vector("el", "perro", "corre")
    assert(fragmentos(palabras, 3) == Vector(Vector("el", "perro", "corre")))
  }

  test("fragmentos: con k igual a 1 separa cada palabra") {
    val palabras = Vector("el", "perro", "corre")
    assert(fragmentos(palabras, 1) ==
      Vector(Vector("el"), Vector("perro"), Vector("corre")))
  }

  // huella --------------------------------------------------------------------

  test("huella: documento sin palabras da huella vacía") {
    val d: Documento = ("d1", Vector())
    assert(huella(d, 2) == Set())
  }

  test("huella: documento con menos palabras que k da huella vacía") {
    val d: Documento = ("d1", Vector("buenos", "dias"))
    assert(huella(d, 3) == Set())
  }

  test("huella: fragmentos repetidos cuentan una sola vez") {
    val d: Documento = ("d1", Vector("si", "no", "si", "no", "si"))
    assert(huella(d, 2) == Set(Vector("si", "no"), Vector("no", "si")))
  }

  test("huella: con k igual a 1 da las palabras sin repetir") {
    val d: Documento = ("d1", Vector("rojo", "azul", "rojo"))
    assert(huella(d, 1) == Set(Vector("rojo"), Vector("azul")))
  }

  test("huella: documento sin repetidos guarda todos sus fragmentos") {
    val d: Documento = ("d1", Vector("sol", "luna", "mar"))
    assert(huella(d, 2) == Set(Vector("sol", "luna"), Vector("luna", "mar")))
  }

  // huellas -------------------------------------------------------------------

  test("huellas: corpus vacío da vector vacío") {
    assert(huellas(Vector(), 2) == Vector())
  }

  test("huellas: corpus con un documento muy corto da conjunto vacío") {
    val d1: Documento = ("d1", Vector("hola"))
    assert(huellas(Vector(d1), 2) == Vector(Set()))
  }

  test("huellas: devuelve una huella por cada documento") {
    val d1: Documento = ("d1", Vector("el", "dia", "esta", "soleado"))
    val d2: Documento = ("d2", Vector("la", "noche", "esta", "fria"))
    assert(huellas(Vector(d1, d2), 2).length == 2)
  }

  test("huellas: mantiene el mismo orden del corpus") {
    val d1: Documento = ("d1", Vector("sol", "luna", "mar"))
    val d2: Documento = ("d2", Vector("sol"))
    assert(huellas(Vector(d1, d2), 2) ==
      Vector(Set(Vector("sol", "luna"), Vector("luna", "mar")), Set()))
  }

  test("huellas: con k igual al largo de cada documento") {
    val d1: Documento = ("d1", Vector("uno", "dos", "tres"))
    val d2: Documento = ("d2", Vector("cuatro", "cinco", "seis"))
    assert(huellas(Vector(d1, d2), 3) ==
      Vector(Set(Vector("uno", "dos", "tres")), Set(Vector("cuatro", "cinco", "seis"))))
  }
}
