summary() <- function(obj){
  if(!inherits(obj,"shape")){
    stop("Summary can only be passed shape objs")
  }
  else{
    UseMethod("summary")
  }
}

summary.2dShape <- function(obj){
  cat(class(obj)[3],"\nperimeter:",perimeter(obj),"\nArea:",area(obj))
}

summary.3dShape <- function(obj){
  cat(class(obj)[3],"\nSurface-area:",area(obj),"\nVolume:",volume(obj))
}
