
#unitCube_floor function

#' An S4 class to represent the function \eqn{\lfloor x_1 + \ldots + x_n \rfloor} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to [0,n],\, \vec{x} \mapsto f(\vec{x}) = \lfloor x_1 + \ldots + x_n \rfloor,}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = \frac{n-1}{2}.}
#'
#' The instance needs to be created with one parameter representing the dimension \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitCube_floor
#' @exportClass unitCube_floor
#' @author Klaus Herrmann
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_floor",dim=n)
unitCube_floor <- setClass(Class="unitCube_floor",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_floor",function(object){
  stopifnot(object@dim>=1)
  return((object@dim-1)/2)
  }
  )

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_floor", x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_floor",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      arg <- rowSums(x)
      floor(arg)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_floor"),
    function(object){
        return(c("unit hypercube","non-continuous","floor"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="unitCube_floor"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_floor"),
    function(object){
    return(c("C.2"))
    }
)

