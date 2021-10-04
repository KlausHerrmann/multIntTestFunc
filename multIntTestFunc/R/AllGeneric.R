
##' Get exact integral for test function instance
##'
##' exactIntegral is a generic function that allows to calculate the exact value of a test function instance over the associated integration domain.
##' @param object The test function that gets evaluated
##' @return Numeric value of the integral of the test function
##' @author Klaus Herrmann
setGeneric("exactIntegral",def=function(object){standardGeneric("exactIntegral")})

##' Check if node points are in the domain of a test function instance
##'
##' domainCheck is a generic function that allows to test if a collection of evaluation points are inside the integration area associated to the test function instance or not.
##' @param object Test function that gets evaluated
##' @param x Matrix where each row represents one evaluation point
##' @return Vector where each element (TRUE or FALSE) indicates if a point (row in the input matrix) is in the integration domain
##' @author Klaus Herrmann
setGeneric("domainCheck",def=function(object,x){standardGeneric("domainCheck")})

##' Check if node points are in the domain of a test function instance ("overload" of domainCheck with additional parameter)
##'
##' domainCheckP is a generic function that allows to test if a collection of evaluation points are inside the integration area associated to the test function instance or not.
##' This "overload" of domainCheck allows to pass a list of additional parameters.
##' @param object Test function that gets evaluated
##' @param x Matrix where each row represents one evaluation point
##' @param param List of additional parameters
##' @return Vector where each element (TRUE or FALSE) indicates if a point (row in the input matrix) is in the integration domain
##' @author Klaus Herrmann
setGeneric("domainCheckP",def=function(object,x,param){standardGeneric("domainCheckP")})

##' Evaluate test function instance for a set of node points
##'
##' evaluate is a generic function that evaluates the test function instance for a collection of evaluation points.
##' @param object Test function that gets evaluated
##' @param x Matrix where each row represents one evaluation point
##' @return Vector where each element is an evaluation of the test function for a node point (row in x)
##' @author Klaus Herrmann
setGeneric("evaluate",def=function(object,x){standardGeneric("evaluate")})

##' Get tags for test function instance
##'
##' getTags is a generic function that returns a vector of tags associated to the test function instance.
##' @param object Test function for which the tags are returned
##' @return Vector with tags related to the function
##' @author Klaus Herrmann
setGeneric("getTags",def=function(object){standardGeneric("getTags")})

##' Get description of integration area for test function instance
##'
##' getIntegrationArea is a generic function that returns a description of the integration area associate to the test function instance.
##' @param object Test function for which the description is returned
##' @return Description of the integration area of the function
##' @author Klaus Herrmann
setGeneric("getIntegrationArea",def=function(object){standardGeneric("getIntegrationArea")})

##' Get references for test function instance
##'
##' getReferences is a generic function that returns a vector of references associated to the test function instance.
##' @param object Test function for which the references are returned
##' @return Vector with references for the specific function
##' @author Klaus Herrmann
setGeneric("getReferences",def=function(object){standardGeneric("getReferences")})
