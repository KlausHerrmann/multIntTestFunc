
#Rn_normalDensity function

#' An S4 class to represent the function \eqn{\frac{1}{\sqrt{(2\pi)^d\det(\Sigma)}}\exp(-((\vec{x}-\vec{\mu})^{\tr}\Sigma^{-1}(\vec{x}-\vec{\mu}))/2)} on \eqn{R^n}
#'
#' Implementation of the function
#' \deqn{f \colon R^n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \frac{1}{\sqrt{(2\pi)^d\det(\Sigma)}}\exp(-((\vec{x}-\vec{\mu})^{\tr}\Sigma^{-1}(\vec{x}-\vec{\mu}))/2),}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{R^n = \times_{i=1}^n R}.
#' In this case the integral is know to be
#' \deqn{\int_{R^n} f(\vec{x}) d\vec{x} = 1.}
#'
#' The instance needs to be created with three parameters representing the dimension \eqn{n}, the location vector \eqn{\vec{\mu}} and the variance-covariance matrix \eqn{\Sigma} which needs to be symmetric positive definite.
#' @slot dim An integer that captures the dimension
#' @slot mean A vector of size dim with real entries. Default is rep(0,dim)
#' @slot sigma A matrix of size dim x dim that is symmetric positive definite. Default is diag(dim)
#' @include AllGeneric.R
#' @export Rn_normalDensity
#' @exportClass Rn_normalDensity
#' @examples
#' n <- as.integer(3)
#' f <- new("Rn_normalDensity",dim=n)
Rn_normalDensity <- setClass(Class="Rn_normalDensity",representation=representation(dim="integer",mean="vector",sigma="matrix"))

#' @rdname exactIntegral
setMethod("exactIntegral","Rn_normalDensity",function(object){
  stopifnot(object@dim>=1)
  return(1.0)
  }
  )

#' @rdname domainCheck
setMethod("domainCheck",c(object="Rn_normalDensity",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkRn(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="Rn_normalDensity",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        y <- mvtnorm::dmvnorm(x=x,mean=object@mean,sigma=object@sigma)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="Rn_normalDensity"),
    function(object){
        return(c("R^n","smooth","normal density","continuous"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="Rn_normalDensity"),
    function(object){
    return("R^n = R x ... x R")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="Rn_normalDensity"),
    function(object){
    return(c("R.3"))
    }
)

