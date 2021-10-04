
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
