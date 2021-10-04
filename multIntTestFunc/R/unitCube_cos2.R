
#unitCube_cos2 function

#' An S4 class to represent the function \eqn{(\cos(\vec{x}\cdot\vec{v}))^2} on \eqn{[0,1]^n}
#'
#' Implementation of the function
#' \deqn{f \colon [0,1]^n \to [0,1],\, \vec{x} \mapsto f(\vec{x}) = (\cos(\vec{x}\cdot\vec{v}))^2,}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{C_n = [0,1]^n} and \eqn{\vec{v}} is a \eqn{n}-dimensional parameter vector where each entry is different from \eqn{0}.
#' The integral is known to be
#' \deqn{\int_{C_n} f(\vec{x}) d\vec{x} = \frac{1}{2}+\frac{1}{2}\cos(\sum_{j=1}^{n}v_j)\prod_{j=1}^{n}\frac{\sin(v_j)}{v_j}.}
#'
#' The instance needs to be created with two parameters representing the dimension \eqn{n} and the \eqn{n}-dimensional parameter vector where each entry is different from \eqn{0}.
#' @slot dim An integer that captures the dimension
#' @slot coeffs A vector of non-zero parameters
#' @examples
#' n <- as.integer(3)
#' f <- new("unitCube_cos2",dim=n, coeffs=c(-1,2,-2))
setClass(Class="unitCube_cos2",representation=representation(dim="integer",coeffs="vector"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitCube_cos2",function(object){
        stopifnot(object@dim>=1)
        stopifnot(length(object@coeffs)==(object@dim), is.numeric(object@coeffs)==TRUE, all(object@coeffs != 0.0)==TRUE)
        return(1/2 + cos(sum(object@coeffs))*prod(sin(object@coeffs)/object@coeffs)/2)
  }
  )

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitCube_cos2",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(length(object@coeffs)==(object@dim), is.numeric(object@coeffs)==TRUE, all(object@coeffs != 0.0)==TRUE)
        checkClosedUnitCube(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitCube_cos2",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(length(object@coeffs)==(object@dim), is.numeric(object@coeffs)==TRUE, all(object@coeffs != 0.0)==TRUE)
        arg <- rowSums(sweep(x, MARGIN=2, object@coeffs, `*`))
        y <- (cos(arg))^2
        return(y)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="unitCube_cos2"),
    function(object){
        return(c("unit hypercube","smooth","cos","continuous"))
    }
)

#' @rdname getIntegrationArea
setMethod("getIntegrationArea",c(object="unitCube_cos2"),
    function(object){
    return("standard unit hypercube: 0 <= x_i <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitCube_cos2"),
    function(object){
    return(c("C.1"))
    }
)

