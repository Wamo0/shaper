draw <- function(shape, color = "blue", fillColor = "transparent") {
  if(!inherits(shape,"shape")){
    stop("Draw can only be passed objects of class shape")
  }
  else{
    UseMethod("draw")
  }
}

#2D shapes
draw.rectangle <- function(rec, color = "blue", fillColor = "transparent") {
  rectangle_data <- data.frame(
    x = c(0, rec$width, rec$width, 0),
    y = c(0, 0, rec$height, rec$height)
  )

  max_dim <- max(rec$height, rec$width)

  p <- ggplot(rectangle_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "Width", y = "Height", title = "Rectangle") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    lims(x = c(0, max_dim), y = c(0, max_dim))

  print(p)
}

draw.triangle <- function(triangle, color = "blue", fillColor = "transparent") {
  triangle_data <- data.frame(
    x = c(0, triangle$a, triangle$b),
    y = c(0, 0, triangle$c)
  )

  max_dim <- max(triangle$a, triangle$b, triangle$c)

  p <- ggplot(triangle_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "Base", y = "Height", title = "Triangle") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    lims(x = c(0, max_dim), y = c(0, max_dim))

  print(p)
}

draw.circle <- function(circle, color = "blue", fillColor = "transparent") {
  circle_data <- data.frame(
    x = circle$radius * cos(seq(0, 2 * pi, length.out = 100)),
    y = circle$radius * sin(seq(0, 2 * pi, length.out = 100))
  )

  p <- ggplot(circle_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "X-axis", y = "Y-axis", title = "Circle") +
    theme_minimal() +
    coord_fixed(ratio = 1)

  print(p)
}


# 3D shapes
draw.sphere <- function(sph, color =  "#FF0000",..) {
  open3d()

  spheres3d(0, 0, 0, radius = sph$radius, color = color, alpha = 0.7)
  decorate3d(box = FALSE ,axes = TRUE)
}

draw.cuboid <- function(cub, color =  "red", ...) {
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

  open3d()
  shade3d(cuboid, color = color, alpha = 0.7)
  wire3d
  decorate3d()
}

draw.cylinder <- function(cyl, color = "blue", ..){
  center <- matrix(c(0, 0, 0, 0, 0, cyl$height), ncol = 3, byrow = TRUE)
  radius <- cyl$radius
  cylinder_mesh <- cylinder3d(center = center, radius = radius, sides = 1000,closed=-2)
  open3d()
  shade3d(cylinder_mesh, color = color, alpha = 0.7)
  decorate3d()
}
