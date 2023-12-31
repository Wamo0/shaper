volume <- function(shp) {
  if(!inherits(shp,"3dShape")){
    stop("Volume can only be passed objects of class 3dShape")
  }
  else{
    UseMethod("volume")
  }
}

volume.cuboid <- function(cu){
  return(cu$depth*cu$height*cu$width)
}

volume.cube <- function(cub){
  return(cub$sideLength * cub$sideLength * cub$sideLength)
}

volume.sphere <- function(sph){
  return((4/3)*pi*sph$radius^3)
}

volume.cylinder <- function(cyl){
  return(pi*cyl$radius^2*cyl$height)
}
