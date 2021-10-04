
#Rn_floorNorm function

#' An S4 class to represent the function \eqn{\frac{\Gamma(n/2+1)}{\pi^{n/2}(1+\lfloor \Vert \vec{x} \Vert_2^n \rfloor)^s}} on \eqn{R^n}
#'
#' Implementation of the function
#' \deqn{f \colon R^n \to [1,\infty),\, \vec{x} \mapsto f(\vec{x}) = \frac{\Gamma(n/2+1)}{\pi^{n/2}(1+\lfloor \Vert \vec{x} \Vert_2^n \rfloor)^s},}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{R^n = \times_{i=1}^n R} and \eqn{s>1} is a paramter.
#' In this case the integral is know to be
#' \deqn{\int_{R^n} f(\vec{x}) d\vec{x} = \zeta(s),}
#' where \eqn{\zeta(s)} is the Riemann zeta function.
#'
#' The instance needs to be created with two parameters representing \eqn{n} and \eqn{s}.
#' @slot dim An integer that captures the dimension
#' @slot s A numeric value bigger than 1 representing a power
#' @examples
#' n <- as.integer(3)
#' f <- new("Rn_floorNorm",dim=n,s=2)
setClass(Class="Rn_floorNorm",representation=representation(dim="integer",s="numeric"))

#' @rdname exactIntegral
setMethod("exactIntegral","Rn_floorNorm",function(object){
  stopifnot(object@dim>=1)
  stopifnot(object@s>1,length(object@s)==1)
  return(pracma::zeta(object@s))
  }
  )

#' @rdname domainCheck
setMethod("domainCheck",c(object="Rn_floorNorm",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(object@s>1,length(object@s)==1)
        checkRn(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="Rn_floorNorm",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(object@s>1,length(object@s)==1)
        const <- gamma(object@dim/2+1)/pi^(object@dim/2)
        y <- const / (floor((sqrt(rowSums(x*x)))^(object@dim))+1)^(object@s)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="Rn_floorNorm"),
    function(object){
        return(c("R^n","non-continuous","floor","norm"))
    }
)

#' @rdname getIntegrationArea
setMethod("getIntegrationArea",c(object="Rn_floorNorm"),
    function(object){
    return("R^n = R x ... x R")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="Rn_floorNorm"),
    function(object){
    return(c("Documentation R.2"))
    }
)

