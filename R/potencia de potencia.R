multiplicacion <- function(a, b) {
  if (b == 0) {
    return(0)
  } else if (b > 0) {
    return(a + multiplicacion(a, b - 1))
  } else {
    return(-multiplicacion(a, -b))
  }
}

exponenciacion <- function(base, exponente) {
  if (exponente == 0) {
    return(1)
  } else if (exponente > 0) {
    return(multiplicacion(base, exponenciacion(base, exponente - 1)))
  } else {
    stop("Exponente debe ser no negativo")
  }
}

calcular <- function(x, y, z) {
  xy <- exponenciacion(x, y)
  return(exponenciacion(xy, z))
}

# Ejemplo
x <- 2
y <- 3
z <- 2
resultado <- calcular(x, y, z)
cat("El resultado de (", x, "^", y, ")^", z, " es: ", resultado, "\n")
