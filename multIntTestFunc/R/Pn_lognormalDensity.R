
#Pn_lognormalDensity function

#' An S4 class to represent the function \eqn{\frac{1}{\prod_{i=1}^{n}x_i \sqrt{(2\pi)^n\det(\Sigma)}}\exp(-((\vec{x}-\vec{\mu})^{T}\Sigma^{-1}(\vec{x}-\vec{\mu}))/2)} on \eqn{[0,\infty)^n}
#'
#' Implementation of the function
#' \deqn{f \colon R^n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \frac{1}{\prod_{i=1}^{n}x_i \sqrt{(2\pi)^n\det(\Sigma)}}\exp(-((\vec{x}-\vec{\mu})^{\tr}\Sigma^{-1}(\vec{x}-\vec{\mu}))/2),}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{[0,\infty)^n = \times_{i=1}^n [0,\infty)}.
#' In this case the integral is know to be
#' \deqn{\int_{R^n} f(\vec{x}) d\vec{x} = 1.}
#'
#' The instance needs to be created with three parameters representing the dimension \eqn{n}, the location vector \eqn{\vec{\mu}} and the variance-covariance matrix \eqn{\Sigma} which needs to be symmetric positive definite.
#' @slot dim An integer that captures the dimension
#' @slot mean A vector of size dim with real entries.
#' @slot sigma A matrix of size dim x dim that is symmetric positive definite.
#' @include AllGeneric.R
#' @export Pn_lognormalDensity
#' @exportClass Pn_lognormalDensity
#' @examples
#' n <- as.integer(3)
#' f <- new("Pn_lognormalDensity",dim=n,mean=rep(0,n),sigma=diag(n))
Pn_lognormalDensity <- setClass(Class="Pn_lognormalDensity",representation=representation(dim="integer",mean="vector",sigma="matrix"))

#' @rdname exactIntegral
setMethod("exactIntegral","Pn_lognormalDensity",function(object){
  stopifnot(object@dim>=1)
  return(1.0)
  }
  )

#' @rdname domainCheck
setMethod("domainCheck",c(object="Pn_lognormalDensity",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkPos(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="Pn_lognormalDensity",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        Jacobian <- apply(x,1,prod)
        ind <- as.numeric(Jacobian > 0)
        y <- mvtnorm::dmvnorm(x=log(x),mean=object@mean,sigma=object@sigma) * ind / Jacobian
    }
)

#' @rdname getTags
setMethod("getTags",c(object="Pn_lognormalDensity"),
    function(object){
        return(c("[0,Inf)^n","smooth","log-normal density","continuous"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="Pn_lognormalDensity"),
    function(object){
    return("[0,Inf)^n = [0,Inf) x ... x [0,Inf)")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="Pn_lognormalDensity"),
    function(object){
    return(c("P.1"))
    }
)

