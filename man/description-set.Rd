% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.r
\name{description<-}
\alias{description<-}
\title{Assign a description to a resource}
\usage{
description(resource) <- value
}
\arguments{
\item{resource}{A vector of \code{ldf_resource}s}

\item{value}{A new resource description}
}
\description{
Replaces the existing description. To add properties to a description use in combination with
\code{\link[=merge_description]{merge_description()}}.
}
\examples{
r <- resource("a")
description(r) <- data.frame(uri="a",label="A")
}
