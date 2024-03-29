% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/domainChecks.R
\name{checkClosedUnitBall}
\alias{checkClosedUnitBall}
\title{Domain check for closed unit ball \eqn{\{\vec{x} \in R^n : \Vert \vec{x} \Vert_{2} \leq 1\}}}
\usage{
checkClosedUnitBall(x)
}
\arguments{
\item{x}{Matrix with numeric entries. Each row represents one point}
}
\value{
Vector where each element (TRUE or FALSE) indicates if a point is in the closed unit ball
}
\description{
The function checks if a point (one row in the input argument) is inside the closed unit ball \eqn{\{\vec{x} \in R^n : \Vert \vec{x} \Vert_2 \leq 1\}} or not.
If the input matrix contains entries that are not numeric, i.e., not representing real numbers, the function throws an error.
The dimension \eqn{n} is automatically inferred from the input matrix and is equal to the number of columns.
}
\examples{
x <- matrix(rnorm(30),10,3)
checkClosedUnitBall(x)
}
\author{
Klaus Herrmann
}
