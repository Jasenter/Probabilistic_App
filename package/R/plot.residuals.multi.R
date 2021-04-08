plot.residuals.multi=function(
### Generates multiple residual diagnostic plots
##title<<  Plots multiple residual diagnostics
##details<< Options for \code{plot.type} include (default is to do all of them):\cr
##\describe{
##  \item{\code{"index"}}{Residuals against the \code{index} arg, see \code{\link{plot.residuals.vs.index}}}
##  \item{\code{"cumsum-index"}}{\code{\link{cumsum}} of residuals against the \code{index} arg, see \code{\link{plot.residuals.vs.index}}}
##  \item{\code{"pred"}}{Residuals against the \code{pred} arg, see \code{\link{plot.residuals.vs.index}}}
##  \item{\code{"prob(pred)"}}{Similar to \code{"pred"} except the cumulative probability of the \code{pred} predictions is plotted on the x-axis instead of value of \code{pred} arg, see \code{\link{plot.residuals.vs.index}}}
##  \item{\code{"qqnorm"}}{Normal QQ plot, see \code{\link{plot.residuals.qqnorm}}}
##  \item{\code{"ACF/PACF"}}{Full and partial autocorrelation, see \code{\link{plot.residuals.acf.pacf}}}
##  \item{\code{"density"}}{Probability density, see \code{\link{plot.residuals.density}}}
##  }
##note<< TODO: add predictive QQ plot, add copula-like plot for relationship between predicted and residuals
residuals, ##<< vector of residuals
index=NULL, ##<< conditioning index, residuals can be plotted against this index (e.g. time or something else), see \code{\link{plot.residuals.vs.index}}
pred=NULL, ##<< model predictions, residuals can be plotted against this vector
cumprob_residuals=NULL, ##<< vector of cumulative probability of each residual, default is \code{NULL}, passed to \code{\link{plot.residuals.pp}}
cumprob_func=pnorm, ##<< function used to transform residuals to cumulative probability, default is \code{pnorm}, passed to \code{\link{plot.residuals.pp}}
plot.type=c("index","ACF/PACF","cumsum-index","pred","prob(pred)","np.sd-pred","np.sd-prob(pred)","qqnorm","pp","density"), ##<< specifices which diagnostics to plot and their order, see details for possible options
col=2, ##<< plot colour of residuals
resid.outlier.thres=c(NA,3.0), ##<< specifies residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}. If a vector is supplied, then multiple sets of diagnostic plots are provided, one for each element of vector \code{resid.outlier.thres}
page.title="",  ##<< page title for plots
user.par=NULL, ##<< passed as an arguement to \code{\link{par}}, if \code{NULL}, the function uses its own specification, see note for details, if \code{NA}, then par is not set at all
reset.par=TRUE, ##<< if \code{TRUE} the original par values are rest upon exit of function, otherwise they are kept,
mask=NULL,  ##<< logical vector used to mask values of \code{obs and pred} from plot. Only values where \code{mask=FALSE} are plotted. Also see note below. If \code{NULL} no mask is applied
main=NULL,
xlab=NULL,
... ##<< additional arguments for plotting, see \code{\link{plot.default}}
){
  if (is.null(index)) {index=seq(1:length(residuals))}
  # Loop over resid.outler.thres
  for (r in 1:length(resid.outlier.thres)){
   # Set-up plotting
   nplot=length(plot.type)

   if (any(!is.null(user.par))){ # if user.par is NA, then do not set-it
     if (!is.na(user.par)) {
       op=par(user.par)
     } else {
    	 if (nplot>=2) op=par(mfrow=c(2,1),oma=c(0,0,3,0),mar=c(4,4,3,2)+0.1)
    	 if (nplot==1) op=par(mfrow=c(1,1),oma=c(0,0,3,0))
       ##note<< if user.par=NULL, then if (nplot>=2) par(mfrow=c(2,1),oma=c(0,0,3,0),mar=c(4,4,3,2)+0.1) OR if (nplot==1) par(mfrow=c(1,1),oma=c(0,0,3,0)) is used
     }
     if (reset.par) on.exit(par(op))
   }
   # Apply mask
   if (any(!is.null(mask))) {
    residuals[mask]=NA
   }
   # Loop over which diagnostics to plot
   for (p in 1:length(plot.type)) {
		 if (plot.type[p]=="index") {
	  		plot.residuals.vs.index(residuals,index=index,col=col,main="Residuals against index",resid.outlier.thres=resid.outlier.thres[r],...)
		 } else if (plot.type[p]=="cumsum-index") {
      if (is.null(mask)) {
       plot.residuals.vs.index(residuals=cumsum(residuals[!is.na(residuals)]),index=index[!is.na(residuals)],col=col,main="Cumulative Sum of Residuals against index",resid.outlier.thres=NA,ylim=c(min(cumsum(residuals[!is.na(residuals)])),max(cumsum(residuals[!is.na(residuals)]))),...)
       plot.residuals.vs.index(residuals=cumsum(residuals[!is.na(residuals)]-mean(residuals[!is.na(residuals)],na.rm=T)),index=index[!is.na(residuals)],col=col,main="Cumulative Sum of (Residuals-E[Residuals]) against index",resid.outlier.thres=NA,...)
       }else{
       ##note<<For cumsum plot, residuals are masked to produce a jump in index number, this means need to hard code the xlimits to the min and max of index vector (without coding)
       plot.residuals.vs.index(residuals=cumsum(residuals[!mask]),index=index[!mask],col=col,main="Cumulative Sum of Residuals against index",resid.outlier.thres=NA,xlim=c(min(index),max(index)),...)
       plot.residuals.vs.index(residuals=cumsum(residuals[!mask]-mean(residuals[!mask],na.rm=T)),index=index[!mask],col=col,main="Cumulative Sum of (Residuals-E[Residuals]) against index",resid.outlier.thres=NA,xlim=c(min(index),max(index)),...)
       }
		 } else if (plot.type[p]=="qqnorm") {
			plot.residuals.qqnorm(residuals,col=col,main="Normal QQ",resid.outlier.thres=resid.outlier.thres[r],...)

		 } else if (plot.type[p]=="pp") {
    	plot.residuals.pp(residuals=residuals,cumprob_residuals=cumprob_residuals,col=col,main="Prob-Prob Plot",ylab="Cumulative probability of residuals",cumprob_func=cumprob_func,resid.outlier.thres=resid.outlier.thres[r],...)
		 } else if (plot.type[p]=="pred") {
       if (is.null(main)){main="Residuals against Predictions"}
       if (is.null(xlab)){xlab="Predictions"}
       plot.residuals.vs.index(residuals,index=pred,col=col,main=main,type="p",xlab=xlab,resid.outlier.thres=resid.outlier.thres[r],...)

		 } else if (plot.type[p]=="prob(pred)") {
      sorted.pred=sort(pred,index.return=TRUE)
      sorted.residuals=residuals[sorted.pred$ix]
  		plot.residuals.vs.index(sorted.residuals,index=seq(1:length(sorted.pred$ix)),col=col,main="Residuals against Prob(Predictions)",type="p",xlab="Cumulative Probability (Predictions)",resid.outlier.thres=resid.outlier.thres[r],prob.x.axis=T,...)

      } else if (plot.type[p]=="ACF/PACF") {
      if (is.null(mask)) {# does not produce ACF if there is a mask
			 plot.residuals.acf.pacf(residuals,main="\nACF/PACF",resid.outlier.thres=resid.outlier.thres[r],...)
			 }
		 } else if (plot.type[p]=="density") {
		   if (is.null(main)){main="Density"}
		   plot.residuals.density(residuals,main=main,xlab=xlab,resid.outlier.thres=resid.outlier.thres[r],col=col,...)
		 } else if (plot.type[p]=="np.sd-pred") {
		  plot.residuals.np.sd(residuals=residuals,pred=pred,resid.outlier.thres=resid.outlier.thres[r],main="Nonparametric SD[residuals]")
		 } else if (plot.type[p]=="np.sd-prob(pred)") {
		   sorted.pred=sort(pred,index.return=TRUE)
		   sorted.residuals=residuals[sorted.pred$ix]
		   plot.residuals.np.sd(residuals=sorted.residuals,pred=seq(1:length(sorted.pred$ix)),resid.outlier.thres=resid.outlier.thres[r],main="Nonparametric SD[residuals]",xlab="Prob(Predictions)",prob.x.axis=T)
		 }
     # Print Titles etc
     if (nplot>1) {
       residual.masked=resid.remove.outliers(residuals=residuals,resid.outlier.thres=resid.outlier.thres[r])
       mtext(outer=T,paste(page.title,"\n",residual.masked$remove.txt))
     } else {
      mtext(outer=T,page.title)
     }
	 }

  }

  invisible()

}

#-----------------------------------------------------------------------------------------
plot.residuals.acf.pacf=function(
### Plot acf and pacf of residuals
##title<< Plot acf and pacf of residuals
##details<< uses \code{\link{acf}} to plot autocorrelation and partial autocorrelation functions of residuals
residuals, ##<< vector of residuals
lag.max=15, ##<< maximum lag for acf, passed to \code{\link{acf}}
ylab='Autocorrelation', ##<< y-axis label,
main='\nACF/PACF', ##<< main title of plot, see \code{\link{plot.default}}
resid.outlier.thres=3.0, ##<< used to specify residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}
...){

  # Remove outliers
  residuals.masked=resid.remove.outliers(residuals,resid.outlier.thres=resid.outlier.thres)
  if(all(is.na(residuals.masked$residuals))) {
    msg=paste("Unable to plot: all(|residuals|)>",resid.outlier.thres,sep="")
    plot.empty(plot.text=msg,main=main,sub=residuals.masked$remove.txt)
    warning(msg); return()
  }

  acf(residuals.masked$residuals,main="",lag.max=lag.max,ylab=ylab,na.action=na.pass,sub=residuals.masked$remove.txt,...)
  cPacf=pacf(residuals.masked$residuals,lag.max=lag.max,plot=F,na.action=na.pass)
	points(cPacf$acf,pch=16, col='red')
	legend('topright',legend=c('acf','pacf'),pch=c(-1, 16),lty=c(1, -1),col=c('black','red'))
  mtext(text=main,line=1,cex=0.8,font=2)

}
#-----------------------------------------------------------------------------------------
plot.residuals.vs.index=function(
### Plot residuals against an index
##title<< Plot residuals against an index
  residuals, ##<< vector of residuals
  index=seq(1:length(residuals)), ##<< conditioning index, residuals can be plotted against this index (e.g. time or something else), default is a numeric sequence from 1 to \code{length(residuals)}
  resid.outlier.thres=3.0, ##<< used to specify residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}
  add=F, ##<< add to existing plot
  xlab="Index No", ##<< x label
  prob.x.axis=F, ##<< logical vector, specifying if you wish index to plotted on a probability axis
  ylab="Residual", ##<< y label
  main="Residuals against index", ##<< main title of plot, see \code{\link{plot.default}}
  sub="", ##<< sub title of plot, see \code{\link{plot.default}}
  type="l", ##<< type of plot, see \code{\link{plot.default}}
  ... ##<< additional plotting arguments passed to \code{\link{plot.default}}

  ){
  # Remove outliers
  residuals.masked=resid.remove.outliers(residuals=residuals,resid.outlier.thres=resid.outlier.thres)
  if(all(is.na(residuals.masked$residuals))) {
      msg=paste("Unable to plot: all(|residuals|)>",resid.outlier.thres,sep="")
      plot.empty(plot.text=msg,main=main,sub=residuals.masked$remove.txt)
      warning(msg); return()
  }
  if(prob.x.axis) { index=(1:length(index))/(length(index)+1) }
  # Undertake plotting
  if (!add) {
    # First plot time series
     plot(index,residuals.masked$residuals,type=type,ylab=ylab,xlab=xlab,main=main,sub=residuals.masked$remove.txt,...)
   } else {
	  lines(index,residuals.masked$residuals,type=type,ylab=ylab,xlab=xlab,main=main,...)
   }


}

#-----------------------------------------------------------------------------------------
plot.residuals.qqnorm=function(
### Plot normal quantile-quantile plot of residuals
##title<< Plot normal quantile-quantile plot of residuals
##details<< Plots a set of residuals on a normal quantile-quantile plot. Residuals that are normally distributed should plot as a straight line on this plot. To verify this, a line is added which represents the quantiles for a theoretical normal distribution, with mean and sd provided by the \code{theoretical} arg.
  residuals, ##<< vector of residuals
  resid.outlier.thres=3.0, ##<< used to specify residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}
  add=F, ##<< add to existing plot
  ylab="Residual", ##<< y label
  theoretical=c(0,sd(residuals,na.rm=T)), ##<< vector of length 2, which represents the mean and sd parameters of a theoretical normal distribution, used to plot the theoretical line on the qqplot (see details). Default   \code{{theoretical=c(0,sd(residuals))}} is the standard normal distribution, with mean=0 and sd=\code{SD[residuals]}. If \code{NULL} on theoretical line is plotted
  ... ##<< additional arguments passed to \code{\link{qqnorm}}

  ){

  # Remove outliers
  residuals.masked=resid.remove.outliers(residuals=residuals,resid.outlier.thres=resid.outlier.thres)

  if(all(is.na(residuals.masked$residuals))) {
    msg=paste("Unable to plot: all(|residuals|)>",resid.outlier.thres,sep="")
    plot.empty(plot.text=msg,sub=residuals.masked$remove.txt,...)
    warning(msg); return()
  }

  # Undertake plotting
  if (!add) {
    # First plot time series
    qqnorm(residuals.masked$residuals,ylab=ylab,sub=residuals.masked$remove.txt,...)
    grid()
   } else {
	 plot.prob(residuals.masked$residuals,add=T,prob.x.axis=qnorm,...)
	}

  if (!is.null(theoretical)){
    qqline(rnorm(n=1e6,mean=theoretical[1],sd=theoretical[2]),col="blue")
    legend(x="topleft",legend=c("Empirical","Theoretical"),col=c("red","blue"),lty=c(NA,"solid"),pch=c(1,NA))
  }
}
#-----------------------------------------------------------------------------------------
plot.residuals.pp=function(
### Plot probability-probability plot of the residuals
##title<< Plot probability-probability plot of the residuals
##details<< Plots the cumulative proabability of a set of residuals on a probability-probability plot. Residuals that match their cumulative that are normally distributed should plot as a straight line on this plot.
  residuals,   ##<< vector of residuals
  cumprob_residuals, ##<< vector of cumulative probability of residuals, if \code{NULL}, cumbprob_func is used
  add=F, ##<< add to existing plot
  ylab="Cumulative Probability of Observed Residual", ##<< y label
  cumprob_func=pnorm, ##<< Function used to transform residuals to cumulative probability, when \code{cumprob_residuals=NULL}, i.e. \code{cumprob_residuals=cumprob_func(residuals)}, default is \code{pnorm}, which assumes that residuals are normally distributed,
  resid.outlier.thres=3.0, ##<< used to specify residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}
  ... ##<< additional arguments passed to \code{\link{qqnorm}}

  ){
  if(is.null(cumprob_residuals)) {# if cumprob_residuals is not supplied, than calculate it
    # First Remove outliers
    residuals.masked=resid.remove.outliers(residuals=residuals,resid.outlier.thres=resid.outlier.thres)

    if(all(is.na(residuals.masked$residuals))) {
      msg=paste("Unable to plot: all(|residuals|)>",resid.outlier.thres,sep="")
      plot.empty(plot.text=msg,sub=residuals.masked$remove.txt,...)
      warning(msg); return()
    }

    # Calculate cumulative probability
    cumprob_residuals=cumprob_func(residuals.masked$residuals)
  }

  # Undertake plotting
  if (!add) {
    # First plot time series
    plot.prob(cumprob_residuals,ylab=ylab,na.action=na.pass,prob.x.axis=qunif,...)
   } else {
   plot.prob(cumprob_residuals,add=T,prob.x.axis=qunif,sub=residuals.masked$remove.txt,...)
	}

  abline(0,1,col="blue")
  legend(x="topleft",legend=c("Empirical","Theoretical"),col=c("red","blue"),lty=c(NA,"solid"),pch=c(1,NA))
}
#-----------------------------------------------------------------------------------------
plot.residuals.density=function(
### Plot empricial probability density of residuals
##title<< Plot empirical probability density of residuals (cf theoretical)
##details<< Uses \code{\link{density}} function to plot probability density function of residuals
## \cr Can also plot the assumed theoretical probability density
  residuals, ##<< vector of residuals
  resid.outlier.thres=3.0, ##<< used to specify residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}
  add=F, ##<< add to existing plot
  na.rm=TRUE, ##<< remove \code{NA}?, passed to \code{\link{density}}
  show.stats=T, ##<< add basic statistics to plot
  xlab="Residuals", ##<< xlab
  col, ##<< colour of residuals to be plotted
  theoretical=list(x=density(residuals,na.rm=T)$x,y=dnorm(density(residuals,na.rm=T)$x,mean=0,sd=sd(residuals,na.rm=T))), ##<< assumed theoretical density, passed as a list with two components x (residual value) and y (theoretical probability density), default is the  normal distribution, mean=0 and sd=\code{SD[residuals]}, if \code{NULL} no theoretical is plotted
  ylim=NULL, ##<< y limits, if \code{NULL} they are set by the max and min of the empirical and theoretical densities (if provided)
  leg.add=T,
  ...  ##<< additional arguments passed to \code{\link{plot.default}}
  ){

  # Remove outliers
  residuals.masked=resid.remove.outliers(residuals=residuals,resid.outlier.thres=resid.outlier.thres)

  if(all(is.na(residuals.masked$residuals))) {
    msg=paste("Unable to plot: all(|residuals|)>",resid.outlier.thres,sep="")
    plot.empty(plot.text=msg,sub=residuals.masked$remove.txt,...)
    warning(msg); return()
  }


  stats.txt=""
  if (show.stats){
   require(moments)
   xlab=paste(xlab,": N=",length(residuals),
                   ", Mean:",format(mean(residuals.masked$residuals,na.rm=T),digits=2),
                   ", SD:",format(sd(residuals.masked$residuals,na.rm=T),digits=2),
                   ", Skew:",format(skewness(residuals.masked$residuals,na.rm=T),digits=2),
                   ", Ex. kurtosis:",format((kurtosis(residuals.masked$residuals,na.rm=T)-3),digits=2),
                   sep="")

  }

  # produce density
  residuals.density=density(residuals.masked$residuals,na.rm=na.rm)
  # Set x.axis limits
  if (is.null(ylim)) {
   if (!is.null(theoretical)) {
    ylim=c(min(residuals.density$y,theoretical$y),max(residuals.density$y,theoretical$y))
   } else {
    ylim=c(min(residuals.density$y),max(residuals.density$y))
   }
  }

  # Undertake plotting
  if (!add) {
    # First plot time series
    plot(residuals.density,xlab=xlab,sub=residuals.masked$remove.txt,col=col,ylim=ylim,...)
   } else {
    lines(residuals.density,...)
	}

  if(!is.null(theoretical)) {
      theoretical=list(x=density(residuals.masked$residuals,na.rm=T)$x,y=dnorm(density(residuals.masked$residuals,na.rm=T)$x,mean=0,sd=sd(residuals.masked$residuals,na.rm=T)))
      # First sort variables
      # plot them
      lines(theoretical$x,theoretical$y,col="blue")
      # add legend
      if (leg.add){
        legend(x="topleft",legend=c("Empirical","Theoretical"),col=c(col,"blue"),lty=c("solid","solid"))
      }
  }

}
#-----------------------------------------------------------------------------------------
plot.residuals.np.sd=function(
  ### Plots non-parametric estimation of sd as function of predictions
  ##title<< Plots non-parametric estimation of sd[residuals] as function of predictions
  ##details<< Uses \code{\link{plot.np.sdy.cond.x}} function to plot np estimate of sd
  residuals, ##<< vector of residuals
  resid.outlier.thres=3.0, ##<< used to specify residual outliers to be excluded from plots, see \code{\link{resid.remove.outliers}}
  prob.x.axis=F,
  pred,
  xlab="Predictions", ##<< xlab
  ylim=NULL, ##<< y limits, if \code{NULL} they are set by the max and min of the empirical and theoretical densities (if provided)
  ...  ##<< additional arguments passed to \code{\link{plot.np.sdy.cond.x}}
){
  if(prob.x.axis) { pred=(1:length(pred))/(length(pred)+1) }
  # Remove outliers
  residuals.masked=resid.remove.outliers(residuals=residuals,resid.outlier.thres=resid.outlier.thres)
  if(all(is.na(residuals.masked$residuals))) {
    msg=paste("Unable to plot: all(|residuals|)>",resid.outlier.thres,sep="")
    plot.empty(plot.text=msg,sub=residuals.masked$remove.txt,...)
    warning(msg); return()
  }
  # Plot np sd[residuals]
  plot.np.sdy.cond.x(x=pred,y=residuals.masked$residuals,xlab=xlab,ylab="Residual",ylim=ylim,...)

}

#-----------------------------------------------------------------------------------------
resid.remove.outliers<- function(
### Remove outliers from residuals, using a threshold or logical vector
##title<< Remove outliers from residuals
residuals, ##<< vector of residuals
resid.mask=NULL, ##<< code{logical vector}, provides a mask of residuals to be removed, all \code{residuals[i]} where \code{residual.mask[i]=TRUE} are set to \code{NA} (missing)
resid.outlier.thres=3.0, ##<< threshold used to remove outliers, expressed in terms of the number of standard deviations, where \code{sd} provides the standard deviations. All residuals \code{>abs(resid.outliers.thres*sd)} are removed, if \code{NA} then no residuals are removed
sd.res=sd(residuals,na.rm=T) ## sd of residuals, used with resid.outlier.thres to remove outliers

){
 NAcount=sum(is.na(residuals))
 if (is.null(resid.mask)) {
  if (!is.na(resid.outlier.thres)) {
    resid.mask=abs(residuals)>(resid.outlier.thres*sd.res)
    remove.txt=paste('N(residuals)>',format((resid.outlier.thres*sd.res),digits=2),'=',sep="")
  } else {
    resid.mask=rep(FALSE,length(residuals))
    remove.txt=""
  }
 }
 residuals[resid.mask]=NA
 n.resid.removed=sum(is.na(residuals))-NAcount
 if (nchar(remove.txt)>0) {remove.txt=paste("Removed ",remove.txt,n.resid.removed,sep="")}
 return(list(residuals=residuals,remove.txt=remove.txt))
### Returns a list with two components
### \describe{
###  \item{\code{residuals}}{vector of residuals with outliers removed}
###  \item{\code{resid.remove.text}}{character string which explains how many outliers removed}}
}

