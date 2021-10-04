
#Rn_Gauss function

#' An S4 class to represent the function \eqn{\exp(-\vec{x}\cdot\vec{x})} on \eqn{R^n}
#'
#' Implementation of the function
#' \deqn{f \colon R^n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \exp(-\vec{x}\cdot\vec{x}) = \exp(-\sum_{i=1}^n x_i^2),}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{R^n = \times_{i=1}^n R}.
#' In this case the integral is know to be
#' \deqn{\int_{R^n} f(\vec{x}) d\vec{x} = \pi^{n/2}.}
#'
#' The instance needs to be created with one parameter representing \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @examples
#' n <- as.integer(3)
#' f <- new("Rn_Gauss",dim=n)
setClass(Class="Rn_Gauss",representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","Rn_Gauss",function(object){
  stopifnot(object@dim>=1)
  return(pi^(object@dim/2))
  }
  )

#' @rdname domainCheck
setMethod("domainCheck",c(object="Rn_Gauss",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
      checkRn(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="Rn_Gauss",x="matrix"),
    function(object,x){
      stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        y <- exp(-rowSums(x*x))
    }
)

#' @rdname getTags
setMethod("getTags",c(object="Rn_Gauss"),
    function(object){
        return(c("R^n","smooth","Gauss","continuous"))
    }
)

#' @rdname getIntegrationArea
setMethod("getIntegrationArea",c(object="Rn_Gauss"),
    function(object){
    return("R^n = R x ... x R")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="Rn_Gauss"),
    function(object){
    return(c("R.1"))
    }
)

