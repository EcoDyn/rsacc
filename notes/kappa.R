#R functions to compute and print na\"{\i}ve statistics, overall and per-class kappa
#   for a square confusion matrix CM
#
#Results are returned in a list, which may be printed with summary.kappa()
#  which also  takes an (optional) argument: the probability of
#  Type I error  (default 0.05)
#
#The matrix must be square (equal number of field, mapped classes)
#You can calculate other statistics from these, e.g. for the average
#   user and producer accuracy something like:
#
#       z<-kappa(x); sum(z$user.naive)/nrow(x); sum(z$prod.naive)/ncol(x)
#
#Author: D G Rossiter, 29-September-2001 Enschede
#           revised 22-April-2004 Ithaca, 28-April-2014 Enschede
#
kappa <- function(CM) {
   #convert both data frames and vectors to matrices
   cmx<-as.matrix(CM)
   #try to convert a vector to a square matrix
   if (ncol(cmx) == 1)
       cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
   nr<-nrow(cmx); nc<-ncol(cmx)
   if (nr != nc)
       { print("Error: matrix is not square"); return(NULL) }
   n<-sum(cmx)
   d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
   th1v<-((th1*(1-th1))/n)
   csum<-apply(cmx,2,sum); rsum<-apply(cmx,1,sum)
   ua<-d/rsum; pa<-d/csum
   th2 <- sum(rsum*csum) / n^2; kh <- (th1-th2)/(1-th2)
   th3 <- sum( (csum + rsum) * d ) / n^2;
   th4 <- 0; for (i in 1:nr) for (j in 1:nc)
        th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2));
   th4 <- th4 / n^3;
   th1c <- 1 - th1; th2c <- 1 - th2;
   khv <- 1/n *
       (     ( ( th1 * th1c ) / th2c^2 ) 
           + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 ) 
           + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 ) 
       )
   #per-class kappa, user's accuracy...
   p <- cmx/n; uap <- apply(p,1,sum); pap <- apply(p,2,sum); dp<-diag(p);
   kpu <- (dp/uap - pap)/(1 - pap);
   #...and its variance
   t1 <- uap-dp; t2 <- (pap*uap)-dp; t3 <- dp*(1 - uap - pap + dp);
   kpuv <- ( (t1/(uap^3 * (1-pap)^3)) * ((t1*t2) + t3) )/n;
   #per-class kappa, producer's reliability...
   kpp <- (dp/pap - uap)/(1 - uap);
   #...and its variance
   t1 <- (pap-dp);
   kppv <- ( (t1/(pap^3 * (1-uap)^3)) * ((t1*t2) + t3) )/n;
   #return all statistics as a list
   return(list(sum.n=n, sum.naive=th1, sum.var=th1v, sum.kappa=kh, sum.kvar=khv,
        user.naive=ua, prod.naive=pa,
        user.kappa=kpu, user.kvar=kpuv, prod.kappa=kpp, prod.kvar=kppv))
}

#R function to print kappa statistics
summary.kappa <- function(kappa, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", kappa$sum.n), quote=F)
  print("Summary of naive statistics", quote=F)
  print(paste(
            "Overall accuracy, stdev, CV%:",
            round(kappa$sum.naive, 4), ",", 
            round(sqrt(kappa$sum.var), 4), ",", 
            round((sqrt(kappa$sum.var)/kappa$sum.naive)*1000,0)/10),
            quote=F)
  w<-ciw(kappa$sum.var, kappa$sum.n)
  print(paste(
            round((1-alpha)*100,0),"% confidence limits for accuracy:",
            round((kappa$sum.naive-w),4),"...",
            round((kappa$sum.naive+w),4)), quote=F, sep="")
  print("User's accuracy", quote=F);  print(round(kappa$user.naive,4));
  print("Producer's reliability:", quote=F); print(round(kappa$prod.naive,4));
  print("Summary of kappa statistics", quote=F)
  print(paste("Overall kappa, stdev, & CV%:",
            round(kappa$sum.kappa,4), ",", 
            round(sqrt(kappa$sum.kvar),4), ",",
            round((sqrt(kappa$sum.kvar)/kappa$sum.kappa)*1000,0)/10), quote=F)
  w<-ciw(kappa$sum.kvar, kappa$sum.n)
  print(paste(
            round((1-alpha)*100,0),"% confidence limits for kappa:",
            round((kappa$sum.kappa-w),4),"...",
            round((kappa$sum.kappa+w),4)), quote=F, sep="")
  print("Per-class kappa, stdev, & CV%, for user's accuracy:", quote=F)
            print(round(kappa$user.kappa,4), quote=F);
            print(round(sqrt(kappa$user.kvar),4), quote=F);
            print(round((sqrt(kappa$user.kvar)/kappa$user.kappa)*1000,0)/10, quote=F);
  print("Per-class kappa, stdev, & CV%, for producer's reliability:", quote=F)
            print(round(kappa$prod.kappa,4), quote=F);
            print(round(sqrt(kappa$prod.kvar),4), quote=F);
            print(round((sqrt(kappa$prod.kvar)/kappa$prod.kappa)*1000,0)/10, quote=F);
}
