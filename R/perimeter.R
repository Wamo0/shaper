perimeter <- function(obj) {
  if(!inherits(obj,"2dShape")){
    stop("perimeter can only be passed 2D hape objs")
  }
  else{
    UseMethod("perimeter")
  }
}

perimeter.rectangle <- function(obj) {
  return(2*obj$length + 2*obj$width)
}

perimeter.circle <- function(obj) {
  return(2*pi*obj$radius)
}

perimeter.triangle <- function(obj) {
  return(obj$a+obj$b+obj$c)
}
