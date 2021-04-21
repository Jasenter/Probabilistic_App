plot.predictiveQQ=function(
### Plot PredictiveQQ diagnostic assessing the reliability of a predictive distribution against an observed data series
##title<< Predictive QQ plot for assessing the reliability of a predictive distribution
##details<< Predictive QQ plots provides a means to assess the reliability of a predictive distribution against an observed data series.
## A reliabile predictive distribution will have observed p-values that plot on the 1:1 line (45 degree). Deviations from the 1:1 line can be interpreted to diagnose deficiencies (see Figure 1 below). See references for further information.
##\cr\cr Options for \code{sym.scale} include:
##\describe{
##  \item{\code{"none"}}{(default) no scaling}
##  \item{\code{"obs"}}{use magnitude of \code{obs} for scaling}
##  \item{\code{"obs.prob"}}{use marginal cumulative probability of observed values for scaling}
##  }
##references<< Thyer, M., B. Renard, D. Kavetski, G. Kuczera, S. W. Franks, and S. Srikanthan (2009), Critical evaluation of parameter consistency and predictive uncertainty in hydrological modeling: A case study using Bayesian total error analysis, Water Resour. Res., 45, W00B14. \url{http://dx.doi.org/10.1029/2008WR006825}
##references<< Laio, F., and S. Tamea (2007), Verification tools for probabilistic forecasts of continuous hydrological variables, Hydrol. Earth Syst. Sci., 11(4),1267- 1277.
##references<< Renard, B., D. Kavetski, G. Kuczera, M. Thyer, and S. W. Franks (2010), Understanding predictive uncertainty in hydrologic modeling: The challenge of identifying input and structural errors, Water Resour. Res., 46(5), W05521. \url{http://dx.doi.org/10.1029/2009WR008328}
   obs.pval=NULL, ##<< numeric vector of observed p-values, if not provided, then uses \code{pred.reps} and \code{obs} to calculate \code{obs.pval}
   pred.reps=NULL, ##<< numeric matrix, replicates of predictions used to form the predictive distribution. Assumed to be in matrix form: \code{[ndata,nreps]} where \code{ndata} is the number of data points and \code{nreps} is the number of replicates
   obs=NULL,      ##<< numeric vector of observed (also used to scale the size of plot symbols - see \code{sym.scale} arg)
   mask=NULL, ##<< logical vector, only plot those values indicated as \code{FALSE} in \code{mask} vector
   obs.auxlog=NULL, ##<< deprecated, set to \code{!mask}, maintained for backwards compatibility
   circle.col=NULL, ##<< plot circles instead of points by specifying colour for circles, \code{NULL} (default) means no circles
   square.col=NULL, ##<< plot squares instead of points by specifying colour for squares, \code{NULL} (default) means no squares
   sym.scale=c("none","obs","obs.prob"), ##<< Specifies how you want to scale size of symbols, see details for options
   inches=0.1,  ##<< numeric scalar, scales symbols size - see \code{\link{symbols}}
   add.line=F,  ##<< logical, add a line to the points, default=\code{FALSE}
   add.pts=T, ##<< logical, add a line to the points, default=\code{TRUE}
   add.grid=T,  ##<< logical, add a grid, default=\code{TRUE}
   add.121.line=T,  ##<< logical, add a 1:1 line, default=\code{TRUE}
   col.121.line = 'black',
   lty.121.line = 1,
   lwd.121.line = 1,
   add.CI=F, ##<<  logical, add confidence limits (CI) to plot, default=\code{FALSE}
   add.indices=T, ##<< logical, add numerical indices for assessing reliability to plot, default=\code{TRUE}
   xlab="Theoretical Quantile of U[0,1]", ##<< x label, default=\code{"Theoretical Quantile of U[0,1]"}
   ylab="Quantile of observed p-value", ##<< y label, default=\code{"Quantile of observed p-value"}
   add=F,
   perturb=F,##<< logical, add to an existing plot
   ...  ##<< additional plot parameters passed to \code{\link{plot.default}}
   ){
   # Check arguments
  if (missing(sym.scale)) {
    sym.scale="none"
  } else {
    sym.scale=match.arg(sym.scale)
  }

  if (perturb){
    nObs = dim(pred.reps)[1]
    nReps = dim(pred.reps)[2]
    pred.reps = pred.reps + matrix(runif(length(pred.reps),0,1e-8),ncol=nReps)
    obs = obs + runif(nObs,0,1e-8)
  }

  # First find the pvals of obs if not input
  if (is.null(obs.pval)) {
    if (!is.null(obs) & !is.null(pred.reps)) {
      obs.pval=calc.obs.pval(pred.reps=pred.reps,obs=obs)
    } else {
      stop("Error - I need some data to plot!! - either obs.pval or obs and pred.reps are required")
    }
  }

  if(!is.null(obs.auxlog)) mask=!obs.auxlog
  if(!is.null(mask)) obs.pval[mask]=NA

  # Remove missing values from observed values
  pval.mv=is.na(obs.pval)
  obs.pval=obs.pval[!pval.mv]


  # Find the theoretical pval of the obs.pval
  y=sort(obs.pval,index.return=T)
  obs=obs[y$ix] # Sort the observed values into the same index
  y=y$x
  n=length(y)
  i=seq(1:n)
  PP=i/(n+1) # Use Weibull Plotting Position
  #browser()
  plotpoints=qqplot(PP,y,plot.it=F)

  # Transfore sym.scale into a numerical value
  if (sym.scale=="none") {
    sym.scale=rep(1,length(obs))
  } else if(sym.scale=="obs"){
    sym.scale=obs
  } else if(sym.scale=="obs.prob"){
    obs_func=ecdf(obs)
    sym.scale=obs_func(obs)
  }

  if (any(sym.scale<0)) sym.scale=sym.scale-min(sym.scale) # Scale the obs to ensure positivity

  # Set-up plot
  if(!add) {
    plot(x=plotpoints$x,y=plotpoints$y,type="n",yaxs="i",xaxs="i",xlim=c(-0.02,1.02),ylim=c(-0.02,1.02),xlab=xlab,ylab=ylab,...)
    if(add.grid) grid(col="grey",lty="dashed")
  }

  # Plot lines first
  if(add.line) lines(x=plotpoints$x,y=plotpoints$y,...)

  # Plot points
  if (add.pts) {
    if (!is.null(circle.col)) {
      symbols(x=plotpoints$x,y=plotpoints$y,circles=sym.scale,bg=circle.col,inches=inches,add=T,...)
    } else if (!is.null(square.col)) {
      symbols(x=plotpoints$x,y=plotpoints$y,squares=sym.scale,bg=square.col,inches=inches,add=T,...)
    } else {
      points(x=plotpoints$x,y=plotpoints$y,...)
    }
  }
  # Any additions to plot
  if(add.121.line) lines(x=c(0,1),y=c(0,1),col=col.121.line,lty=lty.121.line,lwd=lwd.121.line)

  # add confidence limits
  if (add.CI) ecdf.ksCI(plotpoints$x)

  # Numerical Indices
  alpha.prime=mean(abs(plotpoints$y-plotpoints$x))      # (see 23b Renard et al., 2010)
  alpha=1-2*alpha.prime                                 # (see 23a Renard et al., 2010)
  epsilon.prime=mean(plotpoints$y==1|plotpoints$y==0)   # (see 24b Renard et al., 2010)
  epsilon=1-epsilon.prime                               # (see 24a Renard et al., 2010)
  epsilon.high=1-mean(plotpoints$y==1)
  epsilon.low=1-mean(plotpoints$y==0)
  median.bias=0.5-plotpoints$y[round(n/2)]
  criteria.reliability=list(alpha=alpha,epsilon=epsilon,epsilon.high=epsilon.high,epsilon.low=epsilon.low,median.bias=median.bias)
  if (add.indices) {
    text(0.05,0.99,substitute(list(alpha) == list(x),list(x=format(alpha,digits=2))),adj=c(0,0))
    text(0.05,0.93,substitute(list(epsilon) == list(x),list(x=format(epsilon,digits=2))),adj=c(0,0))
    text(0.05,0.88,substitute(list(epsilon[High]) == list(x),list(x=format(epsilon.high,digits=2))),adj=c(0,0))
    text(0.05,0.83,substitute(list(epsilon[Low]) == list(x),list(x=format(epsilon.low,digits=2))),adj=c(0,0))
    text(0.05,0.78,substitute(list(MB) == list(x),list(x=format(median.bias,digits=2))),adj=c(0,0))
  }
  return(invisible(criteria.reliability))
}
#----------------------------------------------------
calc.obs.pval=function(
### Calculates pvals for a vector obs based on a matrix pred.reps
                       pred.reps=NULL,   ##<< a matrix of replicates of predictions(nData,nreps)
                       obs=NULL,        ##<< vector of observed (ndata)
                       pred.reps.ranked=F, ##<< logical denoting whether simulations have already been ranked
                       progress.bar=F    ##<< display the progress bar, default =\code{FALSE}
                      ){
  ndata=nrow(pred.reps)
  obs.pval=vector(length=ndata)

  # create progress bar
  if (progress.bar) pb <- txtProgressBar(min = 0, max = ndata, style = 3)

  for (i in 1:ndata){
    # update progress bar
    if (progress.bar) setTxtProgressBar(pb, i)

    if(is.na(obs[i]) || is.na(pred.reps[i,1])) {
      obs.pval[i]=NA
    } else {
      obs.pval[i]=empirical.pval(y=pred.reps[i,],x=obs[i],ranked=pred.reps.ranked)
    }
  }

  if (progress.bar) close(pb)

  return(obs.pval)
  ### Vector of observed pvalues
}
empirical.pval=function(
### Calculates p-value for \code{x} given an empirical distribution defined by values in \code{y}
y, ##<< vector of values defining the empirical distribution
x, ##<< scalar value x
ranked=F##<< logical, if \code{y} is alread ranked, set this to true.
){

  if (ranked) {  # Have passed in ranked data - altered function ecdf
   Fn=ecdf.ranked(y)
  } else {  # must rank data - standard function

   Fn=ecdf(y)
  }
  pval=Fn(x)
  return(pval)
### returns the p-value or cumulative probability

}
#------------------------------------------------------------------------------------
#
ecdf.ranked=function (x)
### Calculates p-value for x given a empirical distribution defined by values in y (ranked array)
##details<< Straight copy of \code{\link{ecdf}} function with some changes to make it work and avoid a mysterious bug.
## ie. x.coord y.coords lengths to differ
## also allows ranked inputs to save a little bit of time.
## note that ties are handled by mean here, not by 'ordered', as it was causing issues.
## unique was returning multiple occassions of the same value (1) for some reason.

{
    n <- length(x)
    x <- unique(x)
    if (n < 1)
        stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)                          # there is a problem with this routine returning non-unique values sometimes, then causes x.coord y.coords lengths to differ in function below.
    rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
        method = "constant", yleft = 0, yright = 1, f = 0, ties = mean)
    class(rval) <- c("ecdf", "stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

