
#integrate x_1^a_1...x_d^a_d over { x \in R^n : |x|_2 = 1}

#' An S4 class to represent the function \eqn{\prod_{i=1}^n x_i^{a_i}} on \eqn{S^{n-1}}
#'
#' Implementation of the function
#' \deqn{f \colon S^{n-1} \to R,\, \vec{x} \mapsto f(\vec{x}) = \prod_{i=1}^n x_i^{a_i},}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{S^{n-1} = \{\vec{x}\in R^n : \Vert \vec{x} \Vert_2 = 1\}} and \eqn{a_i \in \{0,1,2,3,\ldots\}}, \eqn{i=1,\ldots,n}, are parameters.
#' If at least one of the coefficients \eqn{a_i} is odd, i.e., \eqn{a_i\in\{1,3,5,7,\ldots\}} for at leas one \eqn{i=1,\ldots,n}, the integral is zero, otherwise the integral is known to be
#' \deqn{\int_{S^{n-1}} f(\vec{x}) d\vec{x} = 2\frac{\prod_{i=1}^n\Gamma(b_i)}{\Gamma(\sum_{i=1}^n b_i)},}
#' where \eqn{b_i = (a_i+1)/2}.
#'
#' Due to the difficulty of testing \eqn{\Vert \vec{x} \Vert_2 = 1} in floating point arithmetic this class also implements the function "domainCheckP".
#' This allows to pass a list with an additional non-negative parameter "eps" representing a non-negative real number \eqn{\varepsilon} and allows to test \eqn{1-\varepsilon \leq \Vert \vec{x} \Vert_2 \leq 1+\varepsilon}.
#' See also the documentation of the function "checkUnitSphere" that is used to perform the checks.
#'
#' The instance needs to be created with two parameters representing the dimension \eqn{n} and a \eqn{n}-dimensional vector of integers (including \eqn{0}) representing the exponents.
#' @slot dim An integer that captures the dimension
#' @slot expo An vector that captures the exponents
#' @examples
#' n <- as.integer(3)
#' f <- new("unitSphere_polynomial",dim=n,expo=c(1,2,3))
setClass(Class="unitSphere_polynomial", representation=representation(dim="integer",expo="vector"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitSphere_polynomial",
          function(object){
            stopifnot(object@dim>=1)
            stopifnot(length(object@expo)==object@dim, is.numeric(object@expo)==TRUE, max(object@expo%%1)==0, min(object@expo)>=0)

            #if n=1 we have S^{0} = {-1,1} and hence the integral is 0
            if (object@dim==1){return(0.0)}

            #check if all a_i are even first
            if ( sum(object@expo %% 2) == 0){
                b <- (object@expo+1)/2
                return( 2 * prod( gamma(b) ) / gamma( sum(b) ) )
            }
            else{return(0.0)}
        }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitSphere_polynomial",x="matrix"),
          function(object,x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@expo)==object@dim, is.numeric(object@expo)==TRUE, max(object@expo%%1)==0, min(object@expo)>=0)
            checkUnitSphere(x)
          }
)


#' @rdname domainCheckP
setMethod("domainCheckP",c(object="unitSphere_polynomial", x="matrix", param="list"),
          function(object, x, param){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@expo)==object@dim, is.numeric(object@expo)==TRUE, max(object@expo%%1)==0, min(object@expo)>=0)
            eps <- param$eps
            if (is.null(eps)==TRUE) {eps<-0}
            checkUnitSphere(x,eps)
          }
)


#' @rdname evaluate
setMethod("evaluate",c(object="unitSphere_polynomial",x="matrix"),
          function(object,x){
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
setMethod("getTags",c(object="unitSphere_polynomial"),
          function(object){
            return(c("unit Sphere","polynomial","continuous","smooth"))
          }
)

#' @rdname getIntegrationArea
setMethod("getIntegrationArea",c(object="unitSphere_polynomial"),
          function(object){
            return("standard unit Sphere: ||x||_2 = 1")
          }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitSphere_polynomial"),
          function(object){
            return(c("S.2"))
          }
)


