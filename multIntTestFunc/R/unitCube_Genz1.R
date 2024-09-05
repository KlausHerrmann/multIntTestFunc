
#unitCube_Genz1 function

#' An S4 class to represent the function \eqn{\cos\left(2\pi u + \sum^{n}_{i=1} a_i x_i \right)} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to (-\infty,\infty),\, \vec{x} \mapsto f(\vec{x}) = \cos\left(2\pi u + \sum^{n}_{i=1} a_i x_i \right)},
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = \frac{2^n \cos\left(2\pi u + \sum_{i=1}^{n}a_i/2\right) \prod_{i=1}^n \sin(a_i/2)}{\prod_{i=1}^n a_i}.}
#'
#' The instance needs to be created with three parameter representing the dimension \eqn{n}, the real number \eqn{u} and the vector \eqn{(a_1,...,a_n)}.
#' @slot dim An integer that captures the dimension
#' @slot u A real number representing a shift in the integrand
#' @slot a A vector of real numbers, each non-zero, increasing the difficulty of the integrand with higher absolute values
#' @include AllGeneric.R
#' @export unitCube_Genz1
#' @exportClass unitCube_Genz1
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' u <- pi
#' a <- rep(exp(1),n)
#' f <- new("unitCube_Genz1",dim=n, u=u, a=a)
unitCube_Genz1 <- setClass(Class="unitCube_Genz1",representation=representation(dim="integer",u="numeric",a="vector"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_Genz1",function(object){
    stopifnot(object@dim>=1, length(object@a)==object@dim, length(object@u)==1, all(abs(a)>0)==TRUE)
    return(2^(object@dim)*cos(2*pi*u+sum(a/2))*prod(sin(a/2))/prod(a))
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_Genz1",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1, length(object@a)==object@dim, length(object@u)==1, all(abs(a)>0)==TRUE)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_Genz1",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1, length(object@a)==object@dim, length(object@u)==1, all(abs(a)>0)==TRUE)
        z <- cos(2*pi*object@u + rowSums(sweep(x, MARGIN=2, object@a, "*")))
        return(z)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_Genz1"),
    function(object){
        return(c("unit hypercube","continuous","smooth"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_Genz1"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_Genz1"),
    function(object){
    return(c("C.5","Genz, A., 1984, September. Testing multidimensional integration routines. In Proc. of international conference on Tools, methods and languages for scientific and engineering computation (pp. 81-94)."))
    }
)

