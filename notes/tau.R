#R function to compute the overall tau statistic
#	for a square confusion matrix CM
#	and an (optional) prior probability vector P
#	if missing, P are equal priors for each class
#
#Results are returned in a list
#They may be printed with the other function included here, summary.tau
#
#Error checks: CM must be square; P must have correct number of classes
#	and sum to 1 +/- 0.0001
#
#Author: D G Rossiter, 29-September-2001 Enschede
#           revised 22-April-2004 Ithaca, 28-April-2014 Enschede
#
tau <- function(CM, P) {
  #convert both data frames and vectors to matrices
  cmx<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
  	cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc)
  	{ print("Error: matrix is not square"); return(NULL) }
  #check P and create if necessary
  if (missing(P))
  	P<-rep(1/nr, nr)
  if (length(P) != nc)
  	{ print("Error: prior probabilities vector has wrong length"); return(NULL) }
  if (abs(1-sum(P)) > 0.0001)
  	{ print("Error: prior probabilities must sum to 1"); return(NULL) }
  n<-sum(cmx)
  d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
  csum<-apply(cmx,2,sum); th2<-(csum%*%P)/n
  tau<-(th1-th2)/(1-th2);
  th3<-sum( (csum + (P*n)) * diag(cmx) ) / n^2;
  rsum<-apply(cmx,1,sum)
  ua<-d/rsum; pa<-d/csum
  th4 <- 0; for (i in 1:nr) for (j in 1:nc)
     th4 <- th4 + (cmx[i,j] * ((csum[i] + P[j]*n)^2));
  th4 <- th4 / n^3;
  th1c <- 1 - th1; th2c <- 1 - th2;
  tv <- 1/n *
  	(     ( ( th1 * th1c ) / th2c^2 ) 
    + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 ) 
    + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 ) 
  	)
  return(list(prior=P, obs=rsum, ref=csum, n=n, tau=tau, tvar=tv, coeff=c(th1, th2, th3, th4)))
}

summary.tau <- function(tau, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", tau$n), quote=F)
  print("Prior class probabilities:", quote=F)
  print(tau$prior, quote=F)
  print("Observed class proportions:", quote=F)
  print(round(tau$obs/tau$n,4), quote=F)
  print("Reference class proportions:", quote=F)
  print(round(tau$ref/tau$n,4), quote=F)
  print(paste("Tau, stdev, & CV%:",
    round(tau$tau,4), ",", 
    round(sqrt(tau$tvar),4), ",",
    round((sqrt(tau$tvar)/tau$tau)*1000,0)/10), quote=F)
  w<-ciw(tau$tvar, tau$n)
  print(paste(round((1-alpha)*100,0),"% confidence limits for tau:",
     round((tau$tau-w),4), "...", round((tau$tau+w),4), sep=""), quote=F)
}
