
#Dirichlet integral over simplex

#' An S4 class to represent the function \eqn{\prod_{i=1}^{n}x_i^{v_i-1}(1 - x_1 - \ldots - x_n)^{v_{n+1}-1}} on \eqn{T_n}
#'
#' Implementation of the function
#' \deqn{f \colon T_n \to (0,\infty),\, \vec{x} \mapsto f(\vec{x}) = \prod_{i=1}^{n}x_i^{v_i-1}(1 - x_1 - \ldots - x_n)^{v_{n+1}-1},}
#' where \eqn{n \in \{1,2,3,\ldots\}} is the dimension of the integration domain \eqn{T_n = \{\vec{x} \in \R^n : x_i\geq 0, \Vert \vec{x} \Vert_1 \leq 1\}} and \eqn{v_i>0}, \eqn{i=1,\ldots,n+1}, are constants.
#' The integral is known to be
#' \deqn{\int_{T_n} f(\vec{x}) d\vec{x} = \frac{\prod_{i=1}^{n+1}\Gamma(v_i)}{\Gamma(\sum_{i=1}^{n+1}v_i)},}
#' where \eqn{v_i>0} for \eqn{i=1,\ldots,n+1}.
#'
#' The instance needs to be created with two parameters representing the dimension \eqn{n} and the vector of positive parameters.
#' @slot dim An integer that captures the dimension
#' @slot v A vector of dimension \eqn{n+1} with positive entries representing the constants
#' @include AllGeneric.R
#' @export standardSimplex_Dirichlet
#' @exportClass standardSimplex_Dirichlet
#' @examples
#' n <- as.integer(3)
#' f <- new("standardSimplex_Dirichlet",dim=n,v=c(1,2,3,4))
standardSimplex_Dirichlet <- setClass(Class="standardSimplex_Dirichlet", representation=representation(dim="integer",v="vector"))

#' @rdname exactIntegral
setMethod("exactIntegral","standardSimplex_Dirichlet",
    function(object){
        stopifnot(object@dim>=1)
        stopifnot(length(object@v)==(object@dim+1), is.numeric(object@v)==TRUE, min(object@v)>0)
        prod( gamma(object@v) ) / gamma( sum(object@v) )
    }
)

#' @rdname domainCheck
setMethod("domainCheck",c(object="standardSimplex_Dirichlet",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(length(object@v)==(object@dim+1), is.numeric(object@v)==TRUE, min(object@v)>0)
        checkStandardSimplex(x)
    }
)

#' @rdname evaluate
setMethod("evaluate",c(object="standardSimplex_Dirichlet",x="matrix"),
    function(object,x){
        stopifnot(is.numeric(x)==TRUE, object@dim==ncol(x), object@dim>=1)
        stopifnot(length(object@v)==(object@dim+1), is.numeric(object@v)==TRUE, min(object@v)>0)
        n <- nrow(x)
        d <- ncol(x)
        y <- rep(1,n)
        for (i in 1:d) {
            y <- y * x[,i]^(object@v[i]-1)
        }
        y <- y * ( rep(1,n) - rowSums(x) )^(object@v[d+1]-1)
        return(y)
    }
)

#' @rdname getTags
setMethod("getTags",c(object="standardSimplex_Dirichlet"),
    function(object){
        return(c("simplex","continuous","smooth"))
    }
)

#' @rdname getIntegrationDomain
setMethod("getIntegrationDomain",c(object="standardSimplex_Dirichlet"),
    function(object){
    return("standard unit simplex: x_i >= 0, ||x||_1 <= 1")
    }
)

#' @rdname getReferences
setMethod("getReferences",c(object="standardSimplex_Dirichlet"),
    function(object){
    return(c("T.1"))
    }
)






