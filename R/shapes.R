# Helper function to check if the input is an integer
is.numeric.value <- function(x) {
  is.numeric(x)
}

## 2D Shape Classes
rectangle <- function(height, width) {
  if(!is.numeric.value(height) || !is.numeric.value(width)) {
    stop("Height and width must be numeric values.")
  }
  rect <- list(height = height, width = width)
  class(rect) <- c("shape","2dShape", "rectangle")
  return(rect)
}

square <- function(sideLength){
  if(!is.numeric.value(sideLength)) {
    stop("Side Length must be a numeric value.")
  }
  squ <- rectangle(sideLength, sideLength)
  class(squ) <- c("shape","2dShape","square")
  # Add 'sideLength' attribute for convenience in square-specific calculations
  squ$sideLength <- sideLength
  return(squ)
}

circle <- function(radius){
  if(!is.numeric.value(radius)) {
    stop("Radius must be a numeric value.")
  }
  circ <- list(radius = radius)
  class(circ) <- c("shape","2dShape", "circle")
  return(circ)
}

triangle <- function(a, b, c) {
  if(!is.numeric.value(a) || !is.numeric.value(b) || !is.numeric.value(c)) {
    stop("a, b and c must be numeric values.")
  }
  if(a+b>c & b+c>a & c+a >b){
    tri <- list(a = a, b = b, c = c)
    class(tri) <- c("shape","2dShape", "triangle")
    return(tri)
  }
  stop("Invalid side combos")
}

## 3D Shape Classes
cuboid <- function(height, width, depth) {
  if(!is.numeric.value(height) || !is.numeric.value(width) || !is.numeric.value(depth)) {
    stop("Height, width and depth must be numeric values.")
  }
  cub <- list(width = width, depth = depth, height = height)
  class(cub) <- c("shape","3dShape", "cuboid")
  return(cub)
}

cube <- function(sideLength){
  if(!is.numeric.value(sideLength)) {
    stop("Side Length must be a numeric value.")
  }
  cube <- cuboid(sideLength, sideLength, sideLength)
  class(cube) <- c("shape","3dShape","cube")
  # Add 'sideLength' attribute for convenience in cube-specific calculations
  cube$sideLength <- sideLength
  return(cube)
}

sphere <- function(radius){
  if(!is.numeric.value(radius)) {
    stop("Radius must be a numeric value.")
  }
  sph <- list(radius = radius)
  class(sph) <- c("shape","3dShape", "sphere")
  return(sph)
}

cylinder <- function(radius, height){
  if(!is.numeric.value(radius) || !is.numeric.value(height)) {
    stop("Radius and height must be numeric values.")
  }
  cyl <- list(radius = radius, height = height)
  class(cyl) <- c("shape","3dShape", "cylinder")
  return(cyl)
}
