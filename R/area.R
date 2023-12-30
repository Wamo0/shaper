area <- function(shp) {
  if(!inherits(shp,"shape")){
    stop("area can only be passed objects of class shape")
  }
  else{
    UseMethod("area")
  }
}

#2D
area.rectangle <- function(rect) {
  return(rect$height * rect$width)
}

area.square <- function(squ){
  return(squ$sideLength * squ$sideLength)
}

area.circle <- function(circ) {
  return(pi*circ$radius^2)
}

area.triangle <- function(tri) {
  s <- perimeter(tri)/2
  Area <- sqrt(s*(s-tri$a)*(s-tri$b)*(s-tri$c)) #Heron's formula
  return(Area)
}

#3D - these are surface areas
area.cuboid <- function(cub){
  return(2 * (cub$depth * cub$width + cub$depth * cub$height + cub$height * cub$width))
}

area.cube <- function(cube){
  return(6 * cube$sideLength * cube$sideLength)
}

area.sphere <- function(sph){
  return(4*pi*sph$radius^2)
}

area.cylinder <- function(cyl){
  return(2*pi*cyl$radius*(cyl$radius+cyl$height))
}
