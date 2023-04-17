
#unitCube_BFN4 function

#' An S4 class to represent the function \eqn{\sum_{i=1}^{n}(-1)^i \prod_{j=1}^{i} x_j} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to (-\infty,\infty),\, \vec{x} \mapsto f(\vec{x}) = \sum_{i=1}^{n}(-1)^i \prod_{j=1}^{i} x_j},
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = -(1-(-1/2)^n)/3.}
#'
#' The instance needs to be created with one parameter representing the dimension \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitCube_BFN4
#' @exportClass unitCube_BFN4
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_BFN4",dim=n)
unitCube_BFN4 <- setClass(Class="unitCube_BFN4",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_BFN4",function(object){
    stopifnot(object@dim>=1)          
    return(-(1-(-1/2)^(object@dim))/3)
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_BFN4",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_BFN4",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        signFactor <- (-1)^(1:object@dim)
        cprod <- t(apply(x,1,cumprod))
        z <- cprod%*%signFactor
        return(z)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_BFN4"),
    function(object){
        return(c("unit hypercube","continuous","smooth"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_BFN4"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_BFN4"),
    function(object){
    return(c("C.4","Bratley, P., Fox, B. L., Niederreiter, H. (1992). Implementation and Tests of Low-Discrepancy Sequences. ACM Transactions on Modeling and Computer Simulation, 2(3), 195-213."))
    }
)

