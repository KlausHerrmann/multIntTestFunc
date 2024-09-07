
#unitCube_BFN1 function

#' An S4 class to represent the function \eqn{\prod^{n}_{i=1}\vert 4x_i-2\vert} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to (-\infty,\infty),\, \vec{x} \mapsto f(\vec{x}) = \prod^{n}_{i=1}\vert 4x_i-2\vert},
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = 1.}
#'
#' The instance needs to be created with one parameter representing the dimension \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitCube_BFN1
#' @exportClass unitCube_BFN1
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_BFN1",dim=n)
unitCube_BFN1 <- setClass(Class="unitCube_BFN1",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_BFN1",function(object){
    stopifnot(object@dim>=1)          
    return(1)
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_BFN1",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_BFN1",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        z <- apply(abs(4*x-2),1,prod)
        return(z)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_BFN1"),
    function(object){
        return(c("unit hypercube","continuous","non-differentiable"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_BFN1"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_BFN1"),
    function(object){
    return(c("C.4","Bratley, P., Fox, B. L., Niederreiter, H. (1992). Implementation and Tests of Low-Discrepancy Sequences. ACM Transactions on Modeling and Computer Simulation, 2(3), 195-213."))
    }
)

