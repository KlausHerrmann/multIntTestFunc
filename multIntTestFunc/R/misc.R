
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
#' The function allows to build a multivariate quadrature rule from univariate ones. The multivariate node points are all possible combinations of the univariate node points, and the final weights are the product of the respective univariate weights.
#"
#' @param x Either a list with two elements $nodes and $weights representing a one dimensional quadrature formula which are then used for all dimensions, or a list where each element is a itself a list with two elements $nodes and $weights. In this case the respective quadrature rule is used for each dimension.
#' @param dim An integer that defines the dimension of the output quadrature formula. Default is NULL. If dim is NULL then x has to be a list of quadrature rules (list of lists) and the dimensions is automatically generated. If dim is a positive integer value the same quadrature rule is used in all dimensions.
#' @return A list with a matrix of multivariate node points (each row is one point) and a vector of corresponding weights
#' @examples
#' require(statmod)
#' herm <- gauss.quad(2,"hermite")
#' lag <- gauss.quad(3,"laguerre")
#' qRule1 <- pIntRule(herm,2)
#' qRule2 <- pIntRule(list(herm,lag))
#' @export
pIntRule <- function(x,dim=NULL){
  stopifnot(is.null(dim)||(dim>=1))
  
  if (is.null(dim)){
    qRule <- x
    dim <- length(qRule)
  } else {
    qRule <- vector("list",dim)
    for (k in 1:dim) {
      qRule[[k]] <- x
    }
    
  }
  
  indList <- vector("list",dim)
  
  for (k in 1:dim) {
    indList[[k]] <- 1:length(qRule[[k]]$nodes)
  }
  
  ind <- expand.grid(indList)
  nn <- nrow(ind)
  
  nodes <- matrix(NaN,nn,dim)
  weights <- rep(NaN,nn)
  
  for (k in 1:nn) {
    indk <- as.numeric(ind[k,])
    
    node <- rep(NA,dim)
    logWeight <- 0
    
    for (l in 1:dim){
      i <- indk[l]
      node[l] <- qRule[[l]]$nodes[i]
      logWeight <- logWeight + log(qRule[[l]]$weights[i])
    }
    
    nodes[k,] <- node
    weights[k] <- exp(logWeight)
  }
  
  L <- list(nodes=nodes,weights=weights)
}

