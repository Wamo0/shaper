summary <- function(shp){
  if(!inherits(shp,"shape")){
    stop("This summary function can only be passed objects of class shape")
  }
  else{
    UseMethod("summary")
  }
}

summary.2dShape <- function(shp){
  cat("Shape:",class(shp)[3],"\nPerimeter:",perimeter(shp),"\nArea:",area(shp))
}

summary.3dShape <- function(shp){
  cat("Shape:",class(shp)[3],"\nSurface-area:",area(shp),"\nVolume:",volume(shp))
}

