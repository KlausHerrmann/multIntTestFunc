
#unitCube_max function

#' An S4 class to represent the function \eqn{\max(x_1,\ldots,x_n)} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to [0,n],\, \vec{x} \mapsto f(\vec{x}) = \max(x_1,\ldots,x_n)},
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = \frac{n}{n+1}.}
#'
#' The instance needs to be created with one parameter representing the dimension \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitCube_max
#' @exportClass unitCube_max
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_max",dim=n)
unitCube_max <- setClass(Class="unitCube_max",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_max",function(object){
    stopifnot(object@dim>=1)          
    return(object@dim/(object@dim+1))
    }
    )

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_max",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_max",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        y <- apply(x,1,max)
        return(y)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_max"),
    function(object){
        return(c("unit hypercube","max","non-continuous"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_max"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_max"),
    function(object){
    return(c("C.3","https://math.stackexchange.com/questions/1874340/calculate-the-following-integral?rq=1"))
    }
)

