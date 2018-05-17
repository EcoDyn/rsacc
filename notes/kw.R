#R function to compute na\"{\i}ve statistics, overall and per-class kappa
# for a square confusion matrix CM and a weights matrix W
#  W has 1 on diagonals, [0..1) off; default is no partial credit
#
#Results are returned in a list, which can be printed with summary.kw();
#  this takes an optional prob. of Type I error (alpha, default 0.05)
#
#Error checks: must be square, weights & confusion same size
#
#You can calculate other statistics from these, e.g. for the average
# weighted user's and producer's accuracy something like:
#
#  z<-kw(x); sum(z$user.naive)/nrow(x); sum(z$prod.naive)/ncol(x)
#
#Author: D G Rossiter, 29-August-2001 Enschede
#           revised 22-April-2004 Ithaca; 28-April-2014 Enschede
#
kw <- function(CM, W = diag(sqrt(length(as.matrix(CM)))) ) {
  cmx<-as.matrix(CM); wx<-as.matrix(W)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  if (ncol(wx) == 1)
    wx<-matrix(wx, byrow=TRUE, nrow=sqrt(nrow(wx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc) { print("Error: confusion matrix is not square"); return(NULL) }
  if (dim(wx) != dim(cmx))
    { print("Weight and Confusion Matrices are not the same size"); return(NULL) }
  #summarize cmx
  n<-sum(cmx); rs<-apply(cmx,1,sum); cs<-apply(cmx,2,sum)
  # confusion matrix and marginals as proportions
  p <- cmx/n; cp <- cs/n; rp <- rs/n;
  if ( round((sum(rp) + sum(cp))/2, 2) != 1)
    { print("Error: Bad checksum in row proportions"); return(NULL) }
  # expected proportions
  pp<- rp %o% cp
  # weighted weights
  wr <- wx%*%cp; wc <- t(t(wx)%*%rp);
  # marginal accuracy
  # rows = user's
  ua <- apply(wx*p,1,sum)/rp; uasd<-sqrt(ua*(1-ua)/rs);
  # columns = producer's
  pa <- apply(wx*p,2,sum)/cp; pasd<-sqrt(pa*(1-pa)/cs);
  thw1 <- sum(sum(p * wx)); thw1v<-((thw1*(1-thw1))/n)
  thw2 <- sum(sum(pp * wx));
  khw <- (thw1-thw2)/(1-thw2);
  thw1c <- 1 - thw1; thw2c <- 1 - thw2;
  thw4 <- 0; for (i in 1:nr) for (j in 1:nc)
     thw4 <- thw4 + (p[i,j]*((wx[i,j]*thw2c - (wr[i]+wc[j]) * thw1c)^2 ))
  khwv <- (thw4 - (thw1*thw2 - 2*thw2 + thw1)^2) / (n * thw2c^4)
  return(list(
   sum.n=n,
   sum.kappa=khw, sum.kvar=khwv, theta=c(thw1,thw2,thw4),
   sum.naive=thw1, sum.var=thw1v,
   user.wa=ua, prod.wa=pa,
   user.wsd=uasd, prod.wsd=pasd,
   weights.row=wr, weights.col=wc, expected=pp))
}

summary.kw <- function(kw, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", kw$sum.n), quote=F)
  print(paste("Sum of weighted sum of row, column weights:",
    round(sum(kw$weights.row), 2), ",", 
    round(sum(kw$weights.col), 2) ), quote=F)
  print("Summary of weighted naive statistics", quote=F)
  print(paste(
    "Overall accuracy, stdev, CV%:",
    round(kw$sum.naive, 4), ",", round(sqrt(kw$sum.var), 4), ",", 
    round((sqrt(kw$sum.var)/kw$sum.naive)*1000,0)/10),
    quote=F)
  w<-ciw(kw$sum.var, kw$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for accuracy:",
    round((kw$sum.naive-w),4), "...",
    round((kw$sum.naive+w),4), sep=""), quote=F)
  print("User's weighted accuracy", quote=F)
  print(round(kw$user.wa,4));
  print("Producer's weighted reliability:", quote=F)
  print(round(kw$prod.wa,4));
  print("Summary of weighted kappa statistics", quote=F)
  print(paste("Overall weighted kappa, stdev, & CV%:",
    round(kw$sum.kappa,4), ",", 
    round(sqrt(kw$sum.kvar),4), ",",
    round((sqrt(kw$sum.kvar)/kw$sum.kappa)*1000,0)/10), quote=F)
  w<-ciw(kw$sum.kvar, kw$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for weighted kappa:",
    round((kw$sum.kappa-w),4), "...",
    round((kw$sum.kappa+w),4), sep=""), quote=F)
}
