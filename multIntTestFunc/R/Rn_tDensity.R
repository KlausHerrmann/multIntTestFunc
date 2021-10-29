
#Rn_tDensity function

#' An S4 class to represent the function \eqn{\frac{\Gamma\left[(\nu+n)/2\right]}{\Gamma(\nu/2)\nu^{n/2}\pi^{n/2}\left|{\Sigma}\right|^{1/2}}\left[1+\frac{1}{\nu}({\vec{x}}-{\vec{\delta}})^{T}{\Sigma}^{-1}({\vec{x}}-{\vec{\delta}})\right]^{-(\nu+n)/2}} on \eqn{R^n}
#'
#' Implementation of the function
#' \deqn{f \colon R^n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \frac{\Gamma\left[(\nu+n)/2\right]}{\Gamma(\nu/2)\nu^{n/2}\pi^{n/2}\left|{\Sigma}\right|^{1/2}}\left[1+\frac{1}{\nu}({\vec{x}}-{\vec{\delta}})^{T}{\Sigma}^{-1}({\vec{x}}-{\vec{\delta}})\right]^{-(\nu+n)/2},}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{R^n = \times_{i=1}^n R}.
#' In this case the integral is know to be
#' \deqn{\int_{R^n} f(\vec{x}) d\vec{x} = 1.}
#'
#' The instance needs to be created with four parameters representing the dimension \eqn{n}, the location vector \eqn{\vec{\delta}}, the variance-covariance matrix \eqn{\Sigma} which needs to be symmetric positive definite and the degrees of freedom parameter \eqn{\nu}.

#' @slot dim An integer that captures the dimension
#' @slot delta A vector of size dim with real entries.
#' @slot sigma A matrix of size dim x dim that is symmetric positive definite.
#' @slot df A positive numerical value representing the degrees of freedom.
#' @include AllGeneric.R
#' @export Rn_tDensity
#' @exportClass Rn_tDensity
#' @examples
#' n <- as.integer(3)
#' f <- new("Rn_tDensity",dim=n,delta=rep(0,n),sigma=diag(n),df=3)
Rn_tDensity <- setClass(Class="Rn_tDensity", representation=representation(dim="integer",delta="vector",sigma="matrix",df="numeric"))

#' @rdname exactIntegral
setMethod("exactIntegral","Rn_tDensity",
    function(object){
        stopifnot(object@dim>=1)
        return(1.0)
    }
)


#' @rdname domainCheck
setMethod("domainCheck",c(object="Rn_tDensity",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkRn(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="Rn_tDensity"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        mvtnorm::dmvt(x,delta=object@delta,sigma=object@sigma,df=object@df, log=FALSE, type="shifted") 
    }
)

#' @rdname getTags
setMethod("getTags",c(object="Rn_tDensity"),
    function(object){
        return(c("R^n","continuous","smooth","t-density"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="Rn_tDensity"),
    function(object){
        return("R^n = R x ... x R")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="Rn_tDensity"),
    function(object){
    return(c("R.4"))
    }
)






