
#exp_sum integral over simplex

#' An S4 class to represent the function \eqn{\exp(-c(x_1 + \ldots + x_n))} on \eqn{T_n}
#'
#' Implementation of the function
#' \deqn{f \colon T_n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \exp(-c(x_1 + \ldots + x_n)),}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{T_n = \{\vec{x} \in \R^n : x_i\geq 0, \Vert \vec{x} \Vert_1 \leq 1\}} and \eqn{c>0} is a constant.
#' The integral is known to be
#' \deqn{\int_{T_n} f(\vec{x}) d\vec{x} = \frac{\Gamma(n)-\Gamma(n,c)}{\Gamma(n)c^n},}
#' where \eqn{\Gamma(s,x)} is the incomplete gamma function.
#'
#' The instance needs to be created with two parameters representing the dimension \eqn{n} and the parameter \eqn{c>0}.
#' @slot dim An integer that captures the dimension
#' @slot coeff A strictly positive number representing the constant
#' @include AllGeneric.R
#' @export standardSimplex_exp_sum
#' @exportClass standardSimplex_exp_sum
#' @examples
#' n <- as.integer(3)
#' f <- new("standardSimplex_exp_sum",dim=n,coeff=1)
standardSimplex_exp_sum <- setClass(Class="standardSimplex_exp_sum", representation=representation(dim="integer",coeff="numeric"))

#' @rdname exactIntegral
setMethod("exactIntegral","standardSimplex_exp_sum",
    function(object){
        stopifnot(object@dim>=1)
        stopifnot(length(object@coeff)==1,object@coeff > 0)
        y <- (gamma(object@dim)-gsl::gamma_inc(object@dim,object@coeff)) / ( (object@coeff)^(object@dim) * gamma(object@dim) )
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="standardSimplex_exp_sum",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(length(object@coeff)==1,object@coeff > 0)
        checkStandardSimplex(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="standardSimplex_exp_sum",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(length(object@coeff)==1,object@coeff > 0)
        arg <- rowSums(x)
        y <- exp(-object@coeff*arg)
        return(y)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="standardSimplex_exp_sum"),
    function(object){
        return(c("simplex","continuous","exp","smooth"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="standardSimplex_exp_sum"),
    function(object){
    return("standard unit simplex: x_i >= 0, ||x||_1 <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="standardSimplex_exp_sum"),
    function(object){
    return(c("T.2"))
    }
)






