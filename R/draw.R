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

  spheres3d(0, 0, 0, radius = obj$radius, color = color, alpha = 0.7)
  decorate3d(box = FALSE ,axes = TRUE)
}

draw.cuboid <- function(obj, color =  "red") {
  height <- obj$height
  width <- obj$width
  depth <- obj$depth

  vertices <- cbind(
    c(0,0,0),
    c(0,0, height),
    c(0, depth,0),
    c(0, depth, height),
    c( width,0,0),
    c( width,0, height),
    c( width, depth,0),
    c( width, depth, height)
  )

  indices <- cbind(
    c(1, 5, 7, 3),
    c(2, 6, 8, 4),
    c(1, 2, 4, 3),
    c(5, 6, 8, 7),
    c(3, 7, 8, 4),
    c(1, 5, 6, 2)
  )

  cuboid <- qmesh3d(
    vertices = vertices,
    indices = indices,
    homogeneous = FALSE
  )

  shade3d(cuboid, color = color, alpha = 0.7)
  wire3d
  decorate3d()
}

draw.cylinder <- function(obj){
  center <- matrix(c(0, 0, 0, 0, 0, obj$height), ncol = 3, byrow = TRUE)
  radius <- obj$radius
  cylinder_mesh <- cylinder3d(center = center, radius = radius, sides = 1000,closed=-2)
  open3d()
  shade3d(cylinder_mesh, color = "blue", alpha = 0.7)
  decorate3d()
}
