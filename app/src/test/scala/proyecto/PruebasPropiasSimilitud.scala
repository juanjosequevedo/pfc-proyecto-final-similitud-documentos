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

  // jaccard -------------------------------------------------------------------

  test("jaccard: Tres fragmentos compartidos") {
    assert(jaccard(
      Set(Vector("roca", "firme"), Vector("nube", "gris"), Vector("hoja", "seca")),
      Set(Vector("roca", "firme"), Vector("nube", "gris"), Vector("hoja", "seca"))
    ) == 1.0)
  }

  test("jaccard: Cuatro fragmentos diferentes") {
    assert(jaccard(
      Set(Vector("tren", "viaja"), Vector("barco", "navega")),
      Set(Vector("avion", "vuela"), Vector("bici", "rueda"))
    ) == 0.0)
  }

  test("jaccard: Primera huella vacía") {
    assert(jaccard(
      Set.empty,
      Set(Vector("cobre", "metal"), Vector("plata", "metal"))
    ) == 0.0)
  }

  test("jaccard: Ambas huellas vacías") {
    assert(jaccard(Set.empty, Set.empty) == 0.0)
  }

  test("jaccard: Tres fragmentos comunes entre cinco diferentes") {
    assert(jaccard(
      Set(Vector("pera", "fruta"), Vector("uva", "fruta"), Vector("mango", "fruta")),
      Set(Vector("pera", "fruta"), Vector("uva", "fruta"), Vector("kiwi", "fruta"))
    ) == 0.5)
  }

  // parejas -------------------------------------------------------------------

  test("parejas: Verifica todas las combinaciones de once posiciones") {
    assert(parejas(11) == Vector(
      (0, 1), (0, 2), (0, 3), (0, 4), (0, 5),
      (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
      (1, 2), (1, 3), (1, 4), (1, 5), (1, 6),
      (1, 7), (1, 8), (1, 9), (1, 10),
      (2, 3), (2, 4), (2, 5), (2, 6), (2, 7),
      (2, 8), (2, 9), (2, 10),
      (3, 4), (3, 5), (3, 6), (3, 7), (3, 8),
      (3, 9), (3, 10),
      (4, 5), (4, 6), (4, 7), (4, 8), (4, 9), (4, 10),
      (5, 6), (5, 7), (5, 8), (5, 9), (5, 10),
      (6, 7), (6, 8), (6, 9), (6, 10),
      (7, 8), (7, 9), (7, 10),
      (8, 9), (8, 10),
      (9, 10)
    ))
  }

  test("parejas: Verifica que todas las parejas tengan índices crecientes") {
    assert(parejas(13).forall(p => p._1 < p._2))
  }

  test("parejas: Verifica que no existan parejas repetidas") {
    assert(parejas(12).distinct.length == parejas(12).length)
  }

  test("parejas: Verifica la cantidad de combinaciones para un conjunto grande") {
    assert(parejas(15).length == 105)
  }

  test("parejas: Verifica el orden de generación de las parejas") {
    assert(parejas(6) == Vector(
      (0, 1), (0, 2), (0, 3), (0, 4), (0, 5),
      (1, 2), (1, 3), (1, 4), (1, 5),
      (2, 3), (2, 4), (2, 5),
      (3, 4), (3, 5),
      (4, 5)
    ))
  }

  // similitudes -------------------------------------------------------------------

  test("similitudes: Tres huellas sin fragmentos compartidos") {
    assert(similitudes(
      Vector(
        Set(Vector("lapiz", "roto")),
        Set(Vector("mesa", "grande")),
        Set(Vector("puerta", "abierta"))
      ),
      Vector((0, 1), (0, 2), (1, 2))
    ) == Vector(
      (0, 1, 0.0),
      (0, 2, 0.0),
      (1, 2, 0.0)
    ))
  }

  test("similitudes: Dos huellas completamente iguales") {
    assert(similitudes(
      Vector(
        Set(Vector("cobre", "metal"), Vector("hierro", "metal")),
        Set(Vector("cobre", "metal"), Vector("hierro", "metal"))
      ),
      Vector((0, 1))
    ) == Vector((0, 1, 1.0)))
  }

  test("similitudes: Coincidencia de un fragmento entre tres") {
    assert(similitudes(
      Vector(
        Set(Vector("azucar", "dulce"), Vector("sal", "sabor"), Vector("cafe", "bebida")),
        Set(Vector("azucar", "dulce"))
      ),
      Vector((0, 1))
    ) == Vector((0, 1, 1.0 / 3.0)))
  }

  test("similitudes: Conserva el orden de las parejas recibidas") {
    assert(similitudes(
      Vector(
        Set(Vector("pino", "arbol"), Vector("roble", "arbol")),
        Set(Vector("pino", "arbol")),
        Set(Vector("cedro", "arbol"))
      ),
      Vector((1, 2), (0, 2), (0, 1))
    ) == Vector(
      (1, 2, 0.0),
      (0, 2, 0.0),
      (0, 1, 0.5)
    ))
  }

  test("similitudes: Cuatro huellas con coincidencias diferentes") {
    assert(similitudes(
      Vector(
        Set(Vector("plato", "redondo"), Vector("vaso", "vidrio")),
        Set(Vector("plato", "redondo"), Vector("taza", "ceramica")),
        Set(Vector("vaso", "vidrio"), Vector("taza", "ceramica")),
        Set(Vector("plato", "redondo"), Vector("vaso", "vidrio"))
      ),
      Vector((0, 1), (0, 2), (1, 2), (0, 3))
    ) == Vector(
      (0, 1, 1.0 / 3.0),
      (0, 2, 1.0 / 3.0),
      (1, 2, 1.0 / 3.0),
      (0, 3, 1.0)
    ))
  }

  // indiceInvertido -------------------------------------------------------------------

  test("indiceInvertido: Varias huellas vacías") {
    assert(indiceInvertido(
      Vector(Set.empty, Set.empty, Set.empty, Set.empty)
    ).isEmpty)
  }

  test("indiceInvertido: Documentos con fragmentos independientes") {
    assert(indiceInvertido(
      Vector(
        Set(Vector("cobre", "metal")),
        Set(Vector("plata", "metal")),
        Set(Vector("oro", "metal"))
      )
    ) == Map(
      Vector("cobre", "metal") -> Set(0),
      Vector("plata", "metal") -> Set(1),
      Vector("oro", "metal") -> Set(2)
    ))
  }

  test("indiceInvertido: Un fragmento aparece en tres documentos") {
    assert(indiceInvertido(
      Vector(
        Set(Vector("tecla", "espacio"), Vector("tecla", "enter")),
        Set(Vector("tecla", "espacio"), Vector("tecla", "escape")),
        Set(Vector("tecla", "espacio"), Vector("tecla", "control"))
      )
    ) == Map(
      Vector("tecla", "espacio") -> Set(0, 1, 2),
      Vector("tecla", "enter") -> Set(0),
      Vector("tecla", "escape") -> Set(1),
      Vector("tecla", "control") -> Set(2)
    ))
  }

  test("indiceInvertido: Dos documentos contienen exactamente los mismos fragmentos") {
    assert(indiceInvertido(
      Vector(
        Set(Vector("azul", "claro"), Vector("verde", "oscuro")),
        Set(Vector("azul", "claro"), Vector("verde", "oscuro"))
      )
    ) == Map(
      Vector("azul", "claro") -> Set(0, 1),
      Vector("verde", "oscuro") -> Set(0, 1)
    ))
  }

  test("indiceInvertido: Varios fragmentos aparecen en diferentes combinaciones") {
    assert(indiceInvertido(
      Vector(
        Set(Vector("lunes", "inicio"), Vector("martes", "dia")),
        Set(Vector("lunes", "inicio"), Vector("viernes", "final")),
        Set(Vector("martes", "dia"), Vector("viernes", "final")),
        Set(Vector("lunes", "inicio"), Vector("martes", "dia"))
      )
    ) == Map(
      Vector("lunes", "inicio") -> Set(0, 1, 3),
      Vector("martes", "dia") -> Set(0, 2, 3),
      Vector("viernes", "final") -> Set(1, 2)
    ))
  }

  // detectarDirecto -------------------------------------------------------------------

  test("detectarDirecto: cuenta fragmentos repetidos una sola vez") {
    val c: Corpus = Vector(
      ("a", Vector("a", "b", "a", "b", "a")),
      ("b", Vector("a", "b", "a", "c", "a"))
    )

    val r = detectarDirecto(c, 2, 0.5, 5)

    assert(r.length == 1)
    assert((r.head._1, r.head._2) == (0, 1))
    assert(math.abs(r.head._3 - 0.5) < 1e-9)
  }

  test("detectarDirecto: filtra por umbral dentro de un corpus mixto") {
    val c: Corpus = Vector(
      ("d0", Vector("a", "b", "c", "d")),
      ("d1", Vector("a", "b", "x", "y")),
      ("d2", Vector("p", "q", "r", "s")),
      ("d3", Vector("p", "q", "r", "t"))
    )

    val r = detectarDirecto(c, 2, 0.3, 5)

    assert(r.map(m => (m._1, m._2)) == Vector((2, 3)))
    assert(math.abs(r.head._3 - 0.5) < 1e-9)
  }

  test("detectarDirecto: incluye parejas cuya similitud iguala el umbral") {
    val c: Corpus = Vector(
      ("a", Vector("sol", "luna", "mar", "rio")),
      ("b", Vector("sol", "luna", "nube", "cielo"))
    )

    val r = detectarDirecto(c, 2, 0.2, 5)

    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(r.head._3 - 0.2) < 1e-9)
  }

  test("detectarDirecto: ordena por similitud y respeta el tope p") {
    val c: Corpus = Vector(
      ("a", Vector("a", "b", "c")),
      ("b", Vector("a", "b", "c")),
      ("c", Vector("a", "b", "x")),
      ("d", Vector("q", "r", "s"))
    )

    val r = detectarDirecto(c, 2, 0.3, 2)

    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2)))
    assert(math.abs(r(0)._3 - 1.0) < 1e-9)
    assert(math.abs(r(1)._3 - 1.0 / 3.0) < 1e-9)
  }

  test("detectarDirecto: con huellas vacías y umbral cero devuelve ceros") {
    val c: Corpus = Vector(
      ("a", Vector("sol")),
      ("b", Vector("luna")),
      ("c", Vector("mar"))
    )

    assert(detectarDirecto(c,3,0.0,2) == Vector((0,1,0.0),(0,2,0.0)))
  }

  // detectarConIndice -------------------------------------------------------------------

  test("detectarConIndice: omite documentos que no comparten fragmentos") {
    val c: Corpus = Vector(
      ("a", Vector("sol","luna","nube","mar")),
      ("b", Vector("sol","luna","rio","cielo")),
      ("c", Vector("uno","dos","tres","cuatro"))
    )

    val r = detectarConIndice(c,2,0.15,5)

    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(r.head._3 - 0.2) < 1e-9)
  }

  test("detectarConIndices: no repite una pareja que comparte varios fragmentos") {
    val c: Corpus = Vector(
      ("a", Vector("a","b","c","d")),
      ("b", Vector("a","b","c","e")),
      ("c", Vector("x","y","z"))
    )

    val r = detectarConIndice(c,2,0.5,5)

    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(r.head._3 - 0.5) < 1e-9)
  }

  test("detectarConIndice: ordena empates por posición y respeta el tope p") {
    val c: Corpus = Vector(
      ("a", Vector("a","b")),
      ("b", Vector("x","y")),
      ("c", Vector("a","b")),
      ("d", Vector("x","y"))
    )

    val todas = detectarConIndice(c,2,1.0,5)

    assert(todas == Vector((0,2,1.0),(1,3,1.0)))
    assert(detectarConIndice(c,2,1.0,1) == Vector((0,2,1.0)))
  }

  test("detectarConIndice: funciona con fragmentos de una palabra") {
    val c: Corpus = Vector(
      ("a", Vector("a","b","c")),
      ("b", Vector("a","d","e")),
      ("c", Vector("x","y","z"))
    )

    val r = detectarConIndice(c,1,0.2,5)

    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(r.head._3 - 0.2) < 1e-9)
  }

  test("detectarConIndice: con umbral cero incluye parejas de similitud cero") {
    val c: Corpus = Vector(
      ("a", Vector("sol")),
      ("b", Vector("luna")),
      ("c", Vector("mar"))
    )

    val esperado = Vector((0,1,0.0),(0,2,0.0))

    assert(detectarConIndice(c,2,0.0,2) == esperado)
    assert(detectarConIndice(c,2,0.0,2) ==
    detectarDirecto(c,2,0.0,2))
  }
}


