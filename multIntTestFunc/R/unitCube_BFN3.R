
#unitCube_BFN3 function

#' An S4 class to represent the function \eqn{\prod^{n}_{i=1} T_{\nu(i)}(2x_i-1)} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to (-\infty,\infty),\, \vec{x} \mapsto f(\vec{x}) = \prod^{n}_{i=1} T_{\nu(i)}(2x_i-1)},
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n} and \eqn{T_k} is the Chebyshev polynomial of degree \eqn{k} and \eqn{\nu(i) = (i \mod 4) + 1}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = 0.}
#'
#' The instance needs to be created with one parameter representing the dimension \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitCube_BFN3
#' @exportClass unitCube_BFN3
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_BFN3",dim=n)
unitCube_BFN3 <- setClass(Class="unitCube_BFN3",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_BFN3",function(object){
    stopifnot(object@dim>=1)          
    return(0)
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_BFN3",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_BFN3",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)

        #Chebyshev polynomials T_1, T_2, T_3 and T_4
        cheby <- list(function(x){x},
                      function(x){2*x^2-1},
                      function(x){4*x^3-3*x},
                      function(x){8*x^4-8*x^2+1})

        for (j in 1:(object@dim)) {
            x[,j] <- cheby[[j%%4 + 1]](2*x[,j]-1)
        }

        z <- apply(x,1,prod)
        return(z)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_BFN3"),
    function(object){
        return(c("unit hypercube","continuous","smooth"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_BFN3"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_BFN3"),
    function(object){
    return(c("C.6","Bratley, P., Fox, B. L., Niederreiter, H. (1992). Implementation and Tests of Low-Discrepancy Sequences. ACM Transactions on Modeling and Computer Simulation, 2(3), 195-213."))
    }
)

