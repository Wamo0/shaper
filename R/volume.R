volume <- function(obj) {
  if(!inherits(obj,"3dShape")){
    stop("Volume can only be passed 3d shape objs")
  }
  else{
    UseMethod("volume")
  }
}

volume.cuboid <- function(obj){
  return(obj$length*obj$height*obj$width)
}

volume.sphere <- function(obj){
  return((4/3)*pi*obj$radius^3)
}

volume.cylinder <- function(obj){
  return(pi*obj$radius^2*obj$height)
}
