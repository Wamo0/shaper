% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/area.R
\name{area.triangle}
\alias{area.triangle}
\title{Calculate the area of a triangle using Heron's formula.}
\usage{
\method{area}{triangle}(tri)
}
\arguments{
\item{tri}{An object of class 'shape' representing a triangle with sides a, b, and c.}
}
\value{
The area of the triangle.
}
\description{
Calculate the area of a triangle using Heron's formula.
}
\examples{
# Create a triangle object
triangle <- list(a = 3, b = 4, c = 5)
class(triangle) <- "shape"

# Calculate the area of the triangle
area.triangle(triangle)


}
\seealso{
\code{\link{area}}

Other area functions: 
\code{\link{area.circle}()},
\code{\link{area.cube}()},
\code{\link{area.cuboid}()},
\code{\link{area.cylinder}()},
\code{\link{area.rectangle}()},
\code{\link{area.sphere}()},
\code{\link{area.square}()},
\code{\link{area}()}
}
\concept{area functions}
