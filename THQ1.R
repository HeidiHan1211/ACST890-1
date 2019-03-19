# Question 1
P <- function(C, FV, n, int){
  t <- seq(0.5, n, 0.5)
  P <- sum(C*exp(-int*t))+FV*exp(-int[2*n]*t[n])
  return(P)
}
int <- c(0.1,0.2)
P(10,10,1,int)



