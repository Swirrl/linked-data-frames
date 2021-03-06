% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmd.r
\name{subpredobj_binding}
\alias{subpredobj_binding}
\title{Create Basic Graph Pattern for subject binding labelled properties}
\usage{
subpredobj_binding(subject, properties, optional = F)
}
\arguments{
\item{subject}{A character vector of length 1 with the subject binding}

\item{properties}{A list or data frame with vectors named \code{uri} and \code{label}}

\item{optional}{Should the pattern be wrapped in an optional clause?}
}
\value{
a character vector of basic graph patterns
}
\description{
Creates a basic graph pattern a subject with predicate-object pairs.
The subject binding is interpolated directly, so it should be specified
accordingly (e.g. as a variable \code{"?uri"} or a URI \code{"<http://example.net>"})
The predicate is taken from the property's \code{uri}.
The object is a variable binding made from the property's \code{label}.
Unless the pattern is to be contained in an OPTIONAL clause, it
is terminated with a "." character.
}
