draw <- function(shape) {
  if(!inherits(shape,"shape")){
    stop("Draw can only be passed objects of class shape")
  }
  else{
    UseMethod("draw")
  }
}

draw.rectangle <- function(rect) {
  x <- c(0, 0, rect$width, rect$width, 0)
  y <- c(0, rect$height, rect$height, 0, 0)

  plot(x, y, type = "l", asp = 1)
}

draw.triangle <- function(tri) {
  x <- c(0, tri$b, tri$c, 0)
  y <- c(0, 0, tri$a, 0)

  plot(x, y, type = "l", asp = 1)
}

draw.circle <- function(circ){

  theta <- seq(0, 2 * pi, length.out = 100)

  x <- circ$radius * cos(theta)
  y <- circ$radius * sin(theta)

  plot(x,y, type = "l", asp = 1)
}

draw.sphere <- function(sph, color =  "#FF0000") {
  open3d()

  spheres3d(0, 0, 0, radius = sph$radius, color = color, alpha = 0.7)
  decorate3d(box = FALSE ,axes = TRUE)
}

draw.cuboid <- function(cub, color =  "red") {
  height <- cub$height
  width <- cub$width
  depth <- cub$depth

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

draw.cylinder <- function(cyl){
  center <- matrix(c(0, 0, 0, 0, 0, cyl$height), ncol = 3, byrow = TRUE)
  radius <- cyl$radius
  cylinder_mesh <- cylinder3d(center = center, radius = radius, sides = 1000,closed=-2)
  open3d()
  shade3d(cylinder_mesh, color = "blue", alpha = 0.7)
  decorate3d()
}
