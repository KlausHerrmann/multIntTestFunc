
#integrate exp(-||x||_2^2) / (2*pi)^(n/2) over { x \in R^n : |x|_2 <= 1}


#' An S4 class to represent the function \eqn{\frac{1}{(2\pi)^{n/2}}\exp(-\Vert\vec{x}\Vert_2^2/2)} on \eqn{B^{n}}
#'
#' Implementation of the function
#' \deqn{f \colon B_n \to [0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \frac{1}{(2\pi)^{n/2}}\exp(-\Vert\vec{x}\Vert_2^2/2) = \frac{1}{(2\pi)^{n/2}}\exp(-\frac{1}{2}\sum_{i=1}^n x_i^2),}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{B_n = \{\vec{x}\in R^n : \Vert \vec{x} \Vert_2 \leq 1\}}.
#' In this case the integral is know to be
#' \deqn{\int_{B_n} f(\vec{x}) d\vec{x} = P[Z \leq 1] = F_{\chi^2_n}(1),}
#' where \eqn{Z} follows a chisquare distribution with \eqn{n} degrees of freedom.
#'
#' The instance needs to be created with one parameter representing \eqn{n}.
#' @slot dim An integer that captures the dimension
#' @include AllGeneric.R
#' @export unitBall_normGauss
#' @exportClass unitBall_normGauss
#' @examples
#' n <- as.integer(3)
#' f <- new("unitBall_normGauss",dim=n)
unitBall_normGauss <- setClass(Class="unitBall_normGauss", representation=representation(dim="integer"))

#' @rdname exactIntegral
setMethod("exactIntegral","unitBall_normGauss",
          function(object){
            stopifnot(object@dim>=1)
            stats::pchisq(1,df=object@dim)
          }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="unitBall_normGauss",x="matrix"),
          function(object,x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            checkClosedUnitBall(x)
          }
)

#' @rdname evaluate
setMethod("evaluate",c(object="unitBall_normGauss",x="matrix"),
          function(object,x){
            stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
            y <- rep(1,nrow(x))
            for (k in 1:ncol(x)){
              y <- y * stats::dnorm(x[,k])
            }
            #arg <- rowSums(x*x)
            #y <- exp(-arg/2)/(2*pi)^(object@dim/2)
            return(y)
          }
)

#' @rdname getTags
setMethod("getTags",c(object="unitBall_normGauss"),
          function(object){
            return(c("unit ball","norm","continuous","smooth","normal distribution","Gauss"))
          }
)

#' @rdname getIntegrationArea
setMethod("getIntegrationArea",c(object="unitBall_normGauss"),
          function(object){
            return("standard unit ball: ||x||_2 <= 1")
          }
)

#' @rdname getReferences
setMethod("getReferences",c(object="unitBall_normGauss"),
          function(object){
            return(c("B.1"))
          }
)


