## 2D Shape Classes
rectangle <- function(length, width) {
  rect <- list(length = length, width = width)
  class(rect) <- c("shape","2dShape", "rectangle")
  return(rect)
}

square <- function(sideLength){
  squ <- rectangle(sideLength, sideLength)
  return(squ)
}

circle <- function(radius){
  circ <- list(radius = radius)
  class(circ) <- c("shape","2dShape", "circle")
  return(circ)
}

triangle <- function(a, b, c) {
  if(a+b>c & b+c>a & c+a >b){
    tri <- list(a = a, b = b, c = c)
    class(tri) <- c("shape","2dShape", "triangle")
    return(tri)
  }
  stop("Invalid side combos")
}

## 3D Shape Classes
cuboid <- function(width, length, height) {
  cub <- list(width = width, length = length, height = height)
  class(cub) <- c("shape","3dShape", "cuboid")
  return(cub)
}

cube <- function(sideLength){
  return(cuboid(width = sideLength, length = sideLength))
}

sphere <- function(radius){
  sph <- list(radius = radius)
  class(sph) <- c("shape","3dShape", "sphere")
  return(sph)
}

cylinder <- function(radius, height){
  cyl <- list(radius = radius, height = height)
  class(cyl) <- c("shape","3dShape", "cylinder")
  return(cyl)
}
