perimeter <- function(shp) {
  if(!inherits(shp,"2dShape")){
    stop("perimeter can only be passed objects of class 2dShape")
  }
  else{
    UseMethod("perimeter")
  }
}

perimeter.rectangle <- function(rect) {
  return(2*rect$height + 2*rect$width)
}

perimeter.square <- function(squ){
  return(4 * squ$sideLength)
}

perimeter.circle <- function(circ) {
  return(2*pi*circ$radius)
}

perimeter.triangle <- function(tri) {
  return(tri$a+tri$b+tri$c)
}
