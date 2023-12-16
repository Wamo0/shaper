area <- function(obj) {
  if(!inherits(obj,"shape")){
    stop("area can only be passed shape objs")
  }
  else{
    UseMethod("area")
  }
}

#2D
area.rectangle <- function(obj) {
  return(obj$length * obj$width)
}

area.circle <- function(obj) {
  return(pi*obj$radius^2)
}

area.triangle <- function(obj) {
  s <- perimeter(obj)/2
  Area <- sqrt(s*(s-obj$a)*(s-obj$b)*(s-obj$c)) #Heron's formula
  return(Area)
}

#3D - these are surface areas
area.cuboid <- function(obj){
  return(2 * (obj$depth * obj$width + obj$depth * obj$height + obj$height * obj$width))
}

area.sphere <- function(obj){
  return(4*pi*obj$radius^2)
}

area.cylinder <- function(obj){
  return(2*pi*obj$radius*(obj$radius+obj$height))
}
