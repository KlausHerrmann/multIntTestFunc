
#Pn_logtDensity function

#' An S4 class to represent the function \eqn{(\prod_{i=1}^n x_i^{-1})\frac{\Gamma\left[(\nu+n)/2\right]}{\Gamma(\nu/2)\nu^{n/2}\pi^{n/2}\left|{\Sigma}\right|^{1/2}}\left[1+\frac{1}{\nu}({\log(\vec{x})}-{\vec{\delta}})^{T}{\Sigma}^{-1}({\log(\vec{x})}-{\vec{\delta}})\right]^{-(\nu+n)/2}} on \eqn{[0,\infty)^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,\infty)^n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = (\prod_{i=1}^n x_i^{-1})\frac{\Gamma\left[(\nu+n)/2\right]}{\Gamma(\nu/2)\nu^{n/2}\pi^{n/2}\left|{\Sigma}\right|^{1/2}}\left[1+\frac{1}{\nu}({\log(\vec{x})}-{\vec{\delta}})^{T}{\Sigma}^{-1}({\log(\vec{x})}-{\vec{\delta}})\right]^{-(\nu+n)/2},}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{[0,\infty)^n = \times_{i=1}^n [0,\infty)}.

#' In this case the integral is know to be
#' \deqn{\int_{[0,\infty)^n} f(\vec{x}) d\vec{x} = 1.}
#'
#' The instance needs to be created with four parameters representing the dimension \eqn{n}, the location vector \eqn{\vec{\delta}}, the variance-covariance matrix \eqn{\Sigma} which needs to be symmetric positive definite and the degrees of freedom parameter \eqn{\nu}.

#' @slot dim An integer that captures the dimension
#' @slot delta A vector of size dim with real entries.
#' @slot sigma A matrix of size dim x dim that is symmetric positive definite.
#' @slot df A positive numerical value representing the degrees of freedom.
#' @include AllGeneric.R
#' @export Pn_logtDensity
#' @exportClass Pn_logtDensity
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("Pn_logtDensity",dim=n,delta=rep(0,n),sigma=diag(n),df=3)
Pn_logtDensity <- setClass(Class="Pn_logtDensity", representation=representation(dim="integer",delta="vector",sigma="matrix",df="numeric"))

#' @rdname exactIntegral
setMethod("exactIntegral","Pn_logtDensity",
    function(object){
        stopifnot(object@dim>=1)
        return(1.0)
    }
)


#' @rdname domainCheck
setMethod("domainCheck",c(object="Pn_logtDensity",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkPos(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="Pn_logtDensity"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        Jacobian <- apply(x,1,prod)
        ind <- as.numeric(Jacobian > 0)
        y <- mvtnorm::dmvt(x=log(x),delta=object@delta,sigma=object@sigma,df=object@df, log=FALSE, type="shifted")* ind / Jacobian
    }
)

#' @rdname getTags
setMethod("getTags",c(object="Pn_logtDensity"),
    function(object){
        return(c("[0,Inf)^n","smooth","log-t density","continuous"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="Pn_logtDensity"),
    function(object){
        return("[0,Inf)^n = [0,Inf) x ... x [0,Inf)")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="Pn_logtDensity"),
    function(object){
    return(c("P.2"))
    }
)






