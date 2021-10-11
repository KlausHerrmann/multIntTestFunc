
#Function that computes 2F1 with argument scaling for better results:
#https://stats.stackexchange.com/questions/33451/computation-of-hypergeometric-function-in-r
#for now not needed in the package
#Gauss2F1 <- function(a,b,c,x){
#  if(x>=0 & x<1){
#    gsl::hyperg_2F1(a,b,c,x)
#  }else{
#    gsl::hyperg_2F1(c-a,b,c,1-1/(1-x))/(1-x)^b
#  }
#}


#' Product rule for numerical quadrature from univariate nodes and weights
#'
#' @param dim An integer that defines the dimension of the output quadrature formula
#' @param nodes A vector with univariate node points
#' @param weights A vector with univariate weights
#' @return A list with a matrix of multivariate node points (each row is one point) and a vector of corresponding weights
#' @examples
#' require(statmod)
#' N <- 4; kind <- "hermite"
#' gauss <- gauss.quad(N,kind=kind)
#' w <- gauss$weights
#' x <- gauss$nodes
#' multQuad <- quadratureProductRule(2,x,w)
#' @export
quadratureProductRule <- function(dim,nodes,weights){

    w <- weights
    x <- nodes

    l <- rep(list(1:N), dim)
    ind <- expand.grid(l)

    nn <- nrow(ind)

    nodes <- matrix(NaN,nn,dim)
    weights <- rep(NaN,nn)
        
    for (k in 1:nn) {
        indk <- as.numeric(ind[k,]) 
        nodes[k,] <- x[indk]
        weights[k] <- prod(w[indk])
    }

    L <- list(nodes=nodes,weights=weights)
}




