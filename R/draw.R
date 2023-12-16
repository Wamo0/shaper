draw <- function(obj) {
  if(!inherits(obj,"shape")){
    stop("Draw can only be passed shape objs")
  }
  else{
    UseMethod("draw")
  }
}

draw.rectangle <- function(obj) {
  x <- c(0, 0, obj$width, obj$width, 0)
  y <- c(0, obj$length, obj$length, 0, 0)

  plot(x, y, type = "l", asp = 1)
}

draw.triangle <- function(obj) {
  x <- c(0, obj$b, obj$c, 0)
  y <- c(0, 0, obj$a, 0)

  plot(x, y, type = "l", asp = 1)
}

draw.circle <- function(obj){

  theta <- seq(0, 2 * pi, length.out = 100)

  x <- obj$radius * cos(theta)
  y <- obj$radius * sin(theta)

  plot(x,y, type = "l", asp = 1)
}
