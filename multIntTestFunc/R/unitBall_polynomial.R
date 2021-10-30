
#integrate x_1^a_1...x_d^a_d over { x \in R^n : |x|_2 <= 1}

#' An S4 class to represent the function \eqn{\prod_{i=1}^n x_i^{a_i}} on \eqn{B_n}
#'
#' Implementation of the function
#' \deqn{f \colon B_n \to R,\, \vec{x} \mapsto f(\vec{x}) = \prod_{i=1}^n x_i^{a_i},}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{B_n = \{\vec{x}\in R^n : \Vert \vec{x} \Vert_2 \leq 1\}} and \eqn{a_i \in \{0,1,2,3,\ldots\}}, \eqn{i=1,\ldots,n}, are parameters.
#' If at least one of the coefficients \eqn{a_i} is odd, i.e., \eqn{a_i\in\{1,3,5,7,\ldots\}} for at leas one \eqn{i=1,\ldots,n}, the integral is zero, otherwise the integral is known to be
#' \deqn{\int_{B_n} f(\vec{x}) d\vec{x} = 2\frac{\prod_{i=1}^n\Gamma(b_i)}{\Gamma(\sum_{i=1}^n b_i)(n+\sum_{i=1}^n a_i)},}
#' where \eqn{b_i = (a_i+1)/2}.
#'
#' The instance needs to be created with two parameters representing the dimension \eqn{n} and a \eqn{n}-dimensional vector of integers (including \eqn{0}) representing the exponents.
#' @slot dim An integer that captures the dimension
#' @slot expo An vector that captures the exponents
#' @include AllGeneric.R
#' @export unitBall_polynomial
#' @exportClass unitBall_polynomial
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitBall_polynomial",dim=n,expo=c(1,2,3))
unitBall_polynomial <- setClass(Class="unitBall_polynomial", representation=representation(dim="integer", expo="vector"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitBall_polynomial",
          function(object){
            stopifnot(object@dim>=1)
            stopifnot(length(object@expo)==object@dim, is.numeric(object@expo)==TRUE, max(object@expo%%1)==0, min(object@expo)>=0)
            #check if all a_i are even first
            if ( sum(object@expo %% 2) == 0){
              b <- (object@expo+1)/2
              return( 2 * prod( gamma(b) ) /  (gamma( sum(b) ) * (sum(object@expo)+length(object@expo))  ) )
            }
            else{return(0.0)}
          }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitBall_polynomial",x="matrix"),
          function(object, x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@expo)==object@dim, is.numeric(object@expo)==TRUE, max(object@expo%%1)==0, min(object@expo)>=0)
            checkClosedUnitBall(x)
          }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitBall_polynomial",x="matrix"),
          function(object, x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@expo)==object@dim, is.numeric(object@expo)==TRUE, max(object@expo%%1)==0, min(object@expo)>=0)
            n <- nrow(x)
            d <- ncol(x)
            y <- rep(1,n)
            for (i in 1:d) {
              y <- y * x[,i]^object@expo[i]
            }
            return(y)
          }
)

#' @rdname getTags
setMethod("getTags",c(object="unitBall_polynomial"),
          function(object){
            return(c("unit ball","polynomial","continuous","smooth"))
          }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitBall_polynomial"),
          function(object){
            return("standard unit ball: ||x||_2 <= 1")
          }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitBall_polynomial"),
          function(object){
            return(c("B.2"))
          }
)


