
#integrate x_1^a_1...x_d^a_d over { x \in R^n : |x|_2 = 1}

#' An S4 class to represent the function \eqn{(\vec{x}\cdot\vec{a})(\vec{x}\cdot\vec{b})} on \eqn{S^{n-1}}
#'
#' Implementation of the function
#' \deqn{f \colon S^{n-1} \to R,\, \vec{x} \mapsto f(\vec{x}) = (\vec{x}\cdot\vec{a})(\vec{x}\cdot\vec{b}),}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{S^{n-1} = \{\vec{x}\in R^n : \Vert \vec{x} \Vert_2 = 1\}} and \eqn{\vec{a}} and \eqn{\vec{b}} are two \eqn{n}-dimensional parameter vectors.
#' The integral is known to be
#' \deqn{\int_{S^{n-1}} f(\vec{x}) d\vec{x} = \frac{2\pi^{n/2}(\vec{a}\cdot\vec{b})}{n\Gamma(n/2)},}
#' where \eqn{\vec{a}\in R^n} and \eqn{\vec{b}\in R^n}.
#'
#' Due to the difficulty of testing \eqn{\Vert \vec{x} \Vert_2 = 1} in floating point arithmetic this class also implements the function "domainCheckP".
#' This allows to pass a list with an additional non-negative parameter "eps" representing a non-negative real number \eqn{\varepsilon} and allows to test \eqn{1-\varepsilon \leq \Vert \vec{x} \Vert_2 \leq 1+\varepsilon}.
#' See also the documentation of the function "checkUnitSphere" that is used to perform the checks.
#'
#' The instance needs to be created with three parameters representing the dimension \eqn{n} and the two \eqn{n}-dimensional (real) vectors \eqn{\vec{a}} and \eqn{\vec{b}}.
#' @slot dim An integer that captures the dimension
#' @slot a A \eqn{n}-dimensional real vector
#' @slot b A \eqn{n}-dimensional real vector
#' @examples
#' n <- as.integer(3)
#' f <- new("unitSphere_innerProduct1",dim=n,a=c(1,2,3),b=c(-1,-2,-3))
setClass(Class="unitSphere_innerProduct1", representation=representation(dim="integer", a="vector", b="vector"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitSphere_innerProduct1",
          function(object){
            stopifnot(object@dim>=1)
            stopifnot(length(object@a)==object@dim, is.numeric(object@a)==TRUE,length(object@b)==object@dim, is.numeric(object@b)==TRUE)

            #if n=1 we have S^{0} = {-1,1} and hence the integral is 0
            if (object@dim==1){return(0.0)}

            v <- 2*(pi)^(object@dim/2)/gamma(object@dim/2)
            return(v*sum(object@a*object@b)/object@dim)
        }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitSphere_innerProduct1", x="matrix"),
          function(object, x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@a)==object@dim, is.numeric(object@a)==TRUE,length(object@b)==object@dim, is.numeric(object@b)==TRUE)
            checkUnitSphere(x)
          }
)

#' @rdname domainCheckP
setMethod("domainCheckP",c(object="unitSphere_innerProduct1", x="matrix", param="list"),
          function(object, x, param){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@a)==object@dim, is.numeric(object@a)==TRUE,length(object@b)==object@dim, is.numeric(object@b)==TRUE)
            eps <- param$eps
            if (is.null(eps)==TRUE) {eps<-0}
            checkUnitSphere(x,eps)
          }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitSphere_innerProduct1", x="matrix"),
          function(object, x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            stopifnot(length(object@a)==object@dim, is.numeric(object@a)==TRUE,length(object@b)==object@dim, is.numeric(object@b)==TRUE)
            y <- (x%*%object@a)*(x%*%object@b)
          }
)

#' @rdname getTags
setMethod("getTags",c(object="unitSphere_innerProduct1"),
          function(object){
            return(c("unit Sphere","inner product","continuous","smooth"))
          }
)

#' @rdname getIntegrationArea
setMethod("getIntegrationArea",c(object="unitSphere_innerProduct1"),
          function(object){
            return("standard unit Sphere: ||x||_2 = 1")
          }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitSphere_innerProduct1"),
          function(object){
            return(c("S.1"))
          }
)


