// Definición de la función integracion
def integracion(f: Double => Double, a: Double, b: Double): Double = {
  // Calcular el punto medio
  val xMedio: Double = (a + b) / 2
  // Aplicar la fórmula de Simpson 1/3
  val resultado: Double = (b - a) * (f(a) + 4 * f(xMedio) + f(b)) / 6
  resultado
}
// Función para calcular el error absoluto
def calcularError(valorEsperado: Double, valorObtenido: Double): Double = {
  math.abs(valorEsperado - valorObtenido)
}

/// Ejercicio 1
def f(x: Double): Double= -x*x+8*x-12
integracion(f, 3, 5)
calcularError(7.5, integracion(f, 3 , 5))

/// Ejercicio 2
def f2(x: Double): Double= 3 * Math.pow(x,2)
integracion(f2, 0, 2)
calcularError(8, integracion(f2,0,2))

/// Ejercicio 3
def f3(x: Double): Double= x + (2 * math.pow(x,2)) - (math.pow(x,3)) + (5 *math.pow(x,4))
integracion(f3,-1,1)
calcularError(3.333, integracion(f3,-1,1))

/// Ejercicio 4
def f4(x: Double): Double= (2*x + 1) / (math.pow(x,2)+x)
integracion(f4,1,2)
calcularError(1.09861, integracion(f4,1,2))

/// Ejercicio 5
def f5(x: Double): Double= math.pow(2.71828,x)
integracion(f5,0,1)
calcularError(1.71828, integracion(f5,0,1))

/// Ejercicio 6
def f6(x: Double): Double = 1/ math.sqrt(x-1)
integracion(f6,2,3)
calcularError(0.828427, integracion(f6,2,3))

/// Ejercicio 7
def f7(x: Double): Double = 1/ (1 + math.pow(x,2))
integracion(f7,0,1)
calcularError(0.828427, integracion(f7,0,1))

