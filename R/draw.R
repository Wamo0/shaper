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
  y <- c(0, obj$height, obj$height, 0, 0)

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

draw.sphere <- function(obj, color =  "#FF0000") {
  open3d()

  spheres3d(0, 0, 0, radius = obj$radius, color = color, alpha = 0.8)
  decorate3d(box = FALSE ,axes = TRUE)
}

draw.cuboid <- function(obj) {
  height <- obj$height
  width <- obj$width
  depth <- obj$depth

  vertices <- matrix(c(0, 0, 0,
                       width, 0, 0,
                       width, height, 0,
                       0, height, 0,
                       0, 0, depth,
                       width, 0, depth,
                       width, height, depth,
                       0, height, depth), ncol = 3, byrow = TRUE)

  edges <- matrix(c(1, 2, 2, 3, 3, 4, 4, 1,
                    1, 5, 2, 6, 3, 7, 4, 8,
                    5, 6, 6, 7, 7, 8, 8, 5), ncol = 2, byrow = TRUE)

  plot3d(vertices, type = "n", axes = FALSE, box = FALSE)
  segments3d(vertices[edges[, 1], ], vertices[edges[, 2], ])
  axes3d("bbox", labels = c("x", "y", "z"))
  box3d()
}
