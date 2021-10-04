
#' Domain check for closed unit hypercube \eqn{[0,1]^n}
#'
#' The function checks if a point (one row in the input argument) is inside the closed unit hypercube \eqn{[0,1]^n} or not.
#' If the input matrix contains entries that are not numeric, i.e., not representing real numbers, the function throws an error.
#' The dimension \eqn{n} is automatically inferred from the input matrix and is equal to the number of columns.
#' @param x Matrix with numeric entries. Each row represents one point
#' @return Vector where each element (TRUE or FALSE) indicates if a point is in the unit hypercube
#' @examples
#' x <- matrix(rnorm(30),10,3)
#' checkClosedUnitCube(x)
#' @export
checkClosedUnitCube <- function(x){
  stopifnot(is.numeric(x)==TRUE)
  B1 <- apply(x>=0,1,min)
  B2 <- apply(x<=1,1,min)
  as.logical(pmin(B1,B2))
}

#' Domain check for closed unit ball \eqn{\{\vec{x} \in R^n : \Vert x \Vert_2 \leq 1\}}
#'
#' The function checks if a point (one row in the input argument) is inside the closed unit ball \eqn{\{\vec{x} \in R^n : \Vert x \Vert_2 \leq 1\}} or not.
#' If the input matrix contains entries that are not numeric, i.e., not representing real numbers, the function throws an error.
#' The dimension \eqn{n} is automatically inferred from the input matrix and is equal to the number of columns.
#' @param x Matrix with numeric entries. Each row represents one point
#' @return Vector where each element (TRUE or FALSE) indicates if a point is in the closed unit ball
#' @examples
#' x <- matrix(rnorm(30),10,3)
#' checkClosedUnitBall(x)
#' @export
checkClosedUnitBall <- function(x){
  stopifnot(is.numeric(x)==TRUE)
  sqrt(rowSums( x * x )) <= 1
}

#' Domain check for unit sphere \eqn{\{\vec{x} \in R^n : \Vert x \Vert_2 = 1\}}
#'
#' The function checks if a point (one row in the input argument) is inside the unit sphere \eqn{\{\vec{x} \in R^n : \Vert x \Vert_2 = 1\}} or not.
#' If the input matrix contains entries that are not numeric, i.e., not representing real numbers, the function throws an error.
#' The dimension \eqn{n} is automatically inferred from the input matrix and is equal to the number of columns.
#' The function allows for an additional parameter \eqn{\varepsilon\geq 0} to test \eqn{\{\vec{x} \in R^n : 1-\varepsilon \leq \Vert x \Vert_2 \leq 1 + \varepsilon\}}.
#' WARNING: Due to floating point arithmetic the default value of \eqn{\varepsilon=0} will not work properly in most cases.
#'
#' @param x Matrix with numeric entries. Each row represents one point
#' @param eps Non-negative numeric that allows to test points with an additional tolerance
#' @return Vector where each element (TRUE or FALSE) indicates if a point is in the unit sphere
#' @examples
#' x <- matrix(rnorm(30),10,3)
#' checkUnitSphere(x,eps=0.001)
#' @export
checkUnitSphere <- function(x,eps=0){
  stopifnot(is.numeric(x)==TRUE, is.numeric(eps)==TRUE, length(eps)==1, eps>=0)
  B1 <- sqrt(rowSums( x * x )) <= 1 + eps
  B2 <- sqrt(rowSums( x * x )) >= 1 - eps
  as.logical(B1*B2)
}

#' Domain check for standard simplex \eqn{\{\vec{x} \in R^n : x_i \geq 0, \Vert x \Vert_1 \leq 1 \}}
#'
#' The function checks if a point (one row in the input argument) is inside the standard simplex \eqn{\{\vec{x} \in R^n : x_i \geq, \Vert x \Vert_1 \leq 1 \}} or not.
#' If the input matrix contains entries that are not numeric, i.e., not representing real numbers, the function throws an error.
#' The dimension \eqn{n} is automatically inferred from the input matrix and is equal to the number of columns.
#' @param x Matrix with numeric entries. Each row represents one point
#' @return Vector where each element (TRUE or FALSE) indicates if a point is in the standard simplex
#' @examples
#' x <- matrix(rnorm(30),10,3)
#' checkStandardSimplex(x)
#' @export
checkStandardSimplex <- function(x){
  stopifnot(is.numeric(x)==TRUE)
  #check that x_i >=0 and ||x||_1 <= 1
  B1 <- apply( x >= 0, 1, min)
  B2 <- rowSums( abs(x) ) <= 1
  as.logical( pmin(B1,B2) )
}

#' Domain check for \eqn{R^n }
#'
#' The function checks if a point (one row in the input argument) is inside the n-dimensional Euclidean space \eqn{R^n = \times_{i=1}^n R} or not.
#' In this case the return values are all TRUE.
#' If the input matrix contains entries that are not numeric, i.e., not representing real numbers, the function throws an error.
#' The dimension \eqn{n} is automatically inferred from the input matrix and is equal to the number of columns.
#' @param x Matrix with numeric entries. Each row represents one point
#' @return Vector where each element (TRUE or FALSE) indicates if a point is in R^n
#' @examples
#' x <- matrix(rnorm(30),10,3)
#' checkRn(x)
#' @export
checkRn <- function(x){
  stopifnot(is.numeric(x)==TRUE)
  return(rep(TRUE,length=nrow(x)))
}


