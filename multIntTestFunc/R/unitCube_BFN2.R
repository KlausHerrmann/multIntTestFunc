
#unitCube_BFN2 function

#' An S4 class to represent the function \eqn{\prod^{n}_{i=1} i\cos(ix_i)} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to (-\infty,\infty),\, \vec{x} \mapsto f(\vec{x}) = \prod^{n}_{i=1} i\cos(ix_i)},
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = \prod^{n}_{i=1}\sin(i).}
#'
#' The instance needs to be created with one parameter representing the dimension \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitCube_BFN2
#' @exportClass unitCube_BFN2
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_BFN2",dim=n)
unitCube_BFN2 <- setClass(Class="unitCube_BFN2",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_BFN2",function(object){
    stopifnot(object@dim>=1)          
    return(prod(sin(1:(object@dim))))
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_BFN2",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_BFN2",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        veci <- 1:(object@dim)
        z <- apply(sweep(cos(sweep(x, MARGIN=2, veci, "*")), MARGIN=2, veci, "*"),1,prod)
        return(z)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_BFN2"),
    function(object){
        return(c("unit hypercube","continuous","smooth"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_BFN2"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_BFN2"),
    function(object){
    return(c("C.5","Bratley, P., Fox, B. L., Niederreiter, H. (1992). Implementation and Tests of Low-Discrepancy Sequences. ACM Transactions on Modeling and Computer Simulation, 2(3), 195-213."))
    }
)

