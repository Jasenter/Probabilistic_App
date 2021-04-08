plot.problim<-function(
###Generates x-y plot of probability limits from simulated data, with optional comparison with observed
##title<<  X-Y plot of probability limits from simulated data (cf observed)
##references<< Renard, B., D. Kavetski, G. Kuczera, M. Thyer, and S. W. Franks (2010), Understanding predictive uncertainty in hydrologic modeling: The challenge of identifying input and structural errors, Water Resour. Res., 46(5), W05521. \url{http://dx.doi.org/10.1029/2009WR008328}
##details<< Probability limits are derived either from argument, \code{pred.pl} which is matrix of pre-calculated probability limits or
##  if \code{pred.pl=NULL} (default) then the probability limits are calculated internally using the simulated replicates in \code{pred.reps}
##  Probability limits can be filled, or lines, or bars, see arguments for further details.
  x=NULL,           ##<< Conditioning x data, if \code{NULL} (default), then a sequence of numbers up to \code{ndata)} is used
  obs=NULL,         ##<< Observed y data, if \code{NULL} (default), then no observed data is plotted
  obs.name="Observed", ##<< Name of the observed data, used in the legend
  obs.pch=21,       ##<< symbol for observed y data, default=\code{21}, see \code{link{points}} for further details
  obs.lty=NULL,     ##<< line type for observed y data, if \code{NULL} (default), no line is plotted
  obs.lwd=NULL,     ##<< line width for observed y data if \code{NULL} (default), nothing is plotted
  obs.col="black",  ##<< colour used for symbol/line of observed ydata, default= \code{'black'}
  obs.fill.col="white",  ##<< symbol fill colour for observed, default= \code{'white'}
  pred=NULL,         ##<< predictive data, typically "best estimate"" of predicted data, if \code{NULL} (default), then no best estimate of predicted data is plotted
  pred.name="Predicted", ##<< Name of the predictions of y data, used in the legend
  pred.pch=NULL,      ##<< symbol for predictions of y data, default=\code{NULL}, see \code{link{points}} for further details
  pred.lty="solid",   ##<< line type for predictions of y data, if \code{NULL} no line is plotted, default="solid"
  pred.lwd=2,     ##<< line width for predictions of y data if \code{NULL}, nothing is plotted, default=2.0
  pred.col="black",  ##<< colour used for symbol/line of predictions of y data, default= \code{'black'}
  pred.fill.col="white",  ##<< symbol fill colour for predictions of data, default= \code{'white'}
  pred.pl=NULL,      ##<< matrix of pre-calculated probability limits of predictions, with three columns the lower, middle and upper probability limits of the predicted data,  default is \code{NULL} see details
  pred.reps=NULL,    ##<< replicates of predictions in matrix form: \code{[ndata,nreps]} where \code{ndata} is the number of data points and \code{nreps} is the number of replicates
  date=NULL,
  pl=c(0.90,0.50),  ##<< Specifies the probability limits to be calculated if \code{pred.reps} is input instead of \code{pred.pl}, default= \code{c(0.9,0.5)}, which provides the 90% and 50% probability limits
  pl.name="",     ##<< Name applied to probability limits, used in legend
  pl.fill=T,        ##<< flag specifying whether to fill probability limits with \code{pl.col}
  pl.col=heat.colors(length(pl),alpha=0.75)[length(pl):1], ##<< Colour of probability limits, default = \code{heat.colors(length(pl),alpha=0.75)[length(pl):1]} provides nice red/yellow/orange fill colours
  pl.border=NULL, ##<< border used for the probability limits fill, use \code{NA} if you want no border, default is \code{NULL}, passed to border argument of \code{\link{polygon}}
  ylim=NULL, ##<< y-axis limits, if \code{NULL} (default), they are calculated as the max and min of both the obs data and predicted probability limits
  xlim=NULL, ##<< x-axis limits, if \code{NULL} (default), they are calculated from the x data
  leg.add=T, ##<< add a legend, default=\code{TRUE}
  leg.x="topleft",  ##<< Position of legend, see \code{\link{legend}}
  leg.cex.text=1.0, ##<< Size factor for legend text, see \code{\link{legend}}
  leg.bty='o',      ##<< determine whether legend box is drawn and filled, default =\code{'o'}, which is it will be filled, to turn it off, use \code{'n'}
  add=FALSE,        ##<< Add data to existing plot, default =\code{FALSE}
  add.indices=T,    ##<< add numerical indices for assessing the precision of the probability plot, default=\code{TRUE}, see note for details
  switch.xy=F,      ##<< Flag to switch x and y axis, default=\code{FALSE} - if switched the ylim becomes the xlim and vice versa
  pl.bar=F,         ##<< Flag if \code{true}, it will plot barplots for probability limits, instead of lines and fill
  mask=NULL,        ##<< logical vector used to mask values of \code{obs and pred} from plot. Only values where \code{mask=FALSE} are plotted. Also see note below.
  mask.apply="obs",  ##<< character vector specifying what variables mask should apply to. Options include "obs" (default) and "pred" and in future releases, "pred.pl".
  x.axis.timestep="daily",  ##<< If x is of class \code{\link{POSIXct}}, this provides the time step used for tickmarks on the  x axis, it is passed to the \code{by} arg for \code{\link{seq.Date}}, where axis is produced by the command: \code{axis.POSIXct(1, at=seq(min(x),max(x), by=x.axis.timestep), format=x.axis.format)}, see \code{\link{seq.Date}} and \code{\link{axis.POSIXct}} for options. Default = \code{"month"}
  x.axis.format='%Y/%m', ##<< If x is of class \code{\link{POSIXct}}, this provides the format for x-axis labels, see \code{x.axis.timestep} arg for further explanation, see \code{\link{strptime}} for further options
  xtype="date",
   ...              ##<< additional arguments for plotting, see \code{\link{plot.default}}
){
  # Define data for plot
  percentiles=calc.percentiles.from.pl(pl)
  n.pl=length(pl)
  if(is.null(pred.pl)) {pred.pl=calc.problim(pred.reps,percentiles=percentiles,pl.type="ts")}
  if(pl.bar) {median.pl=calc.problim(pred.reps,percentiles=0.5,pl.type="ts")}
  if(is.null(x)) {x<-seq(1,nrow(pred.pl))}
  if(pl.name!="") {plname=paste(pl.name,": ",sep="")}

  # Apply mask
  if (!is.null(mask)) {
    if ("obs" %in% mask.apply) obs[mask]=NA
    if ("pred" %in% mask.apply) pred[mask]=NA
  }

  # Define axis limits
  if (is.null(ylim)) {ylim=c(min(ylim,obs,pred.pl,pred,na.rm=T),max(ylim,obs,pred.pl,pred,na.rm=T))}
  if (is.null(xlim)) {xlim=c(min(x),max(x))}
  if (!is.null(obs)) {obs[obs<ylim[1]]=NA}
  pred.pl[pred.pl<ylim[1]]=ylim[1]
  # Set-up plot
  if(switch.xy) {temp=xlim;xlim=ylim;ylim=temp;rm(temp)}
  if(!add) {plot(x,pred.pl[,1],type="n",ylim=ylim,xlim=xlim,xaxt="n",...)}

  if(any(class(x)=="POSIXct")) {
    axis.POSIXct(1, at=seq(min(x),max(x), by=x.axis.timestep), format=x.axis.format)
  } else if (xtype=="index"){
    axis(1)
  } else if (xtype=="date") {
    axis(side=1,at=seq(from=1,to=length(obs),by=100),labels=date[seq(from=1,to=length(obs),by=100)])
  }

  # Draw Probability Limits
  n=max(length(x),nrow(pred.pl))
  ncol.pl=ncol(pred.pl)
  leg.pch=NULL;leg.col=NULL; leg.lty=NULL; leg.txt=NULL; pt.bg=NULL
  for (i in 1:n.pl) {
    leg.txt=c(paste(pl.name,pl[i]*100,"% probability",sep=""),leg.txt)
    pl.top.col=(ncol.pl-i+1)
    pl.bot.col=i
    if (pl.fill) { # Draw a filled polygon
      x.poly=c(x[1:n],x[n:1])
      poly.bot=pred.pl[1:n,pl.bot.col]
      poly.top=pred.pl[n:1,pl.top.col]
      y.poly=c(poly.bot,poly.top)
      if(!switch.xy) {polygon(x.poly,y.poly,col=pl.col[i],border=pl.border)} else {polygon(y.poly,x.poly,col=pl.col[i],border=pl.border)}
      leg.pch=c(15,leg.pch);leg.col=c(pl.col[i],leg.col);leg.lty=c(NA,leg.lty)
      pt.bg=c(pt.bg,NA)
    } else if (pl.bar) {       # Draw vertical bars for pl
      pkg.ext=require(gplots)
      if (!pkg.ext) {stop("package gplots is required for pl.bar, but is unavailable")}
      if(!switch.xy) {
        mask=which(!(pred.pl[,pl.bot.col]==pred.pl[,pl.top.col]))
        plotCI(x=x[mask],y=median.pl[mask],li=pred.pl[mask,pl.bot.col],ui=pred.pl[mask,pl.top.col],barcol=pl.col[i],col=pl.col[i],add=T)
      } else {
        warning("switch.xy argument not implemented for pl.bar=TRUE")
        ##note<< \code{switch} argument not implemented for \code{pl.bar=TRUE}
      }
      leg.pch=c(leg.pch,NA);leg.lty=c(leg.lty,"dotted");leg.col=c(leg.col,pl.col[i])
      pt.bg=c(pt.bg,NA)
    } else {       # Draw lines
      if(!switch.xy) {
          lines(x=x,y=pred.pl[,pl.bot.col],type="l",lty="dotted",col=pl.col[i])
          lines(x=x,y=pred.pl[,pl.top.col],type="l",lty="dotted",col=pl.col[i])
      } else {
        lines(x=pred.pl[,pl.bot.col],y=x,type="l",lty="dotted",col=pl.col[i])
        lines(x=pred.pl[,pl.top.col],y=x,type="l",lty="dotted",col=pl.col[i])
      }
      leg.pch=c(leg.pch,NA);leg.lty=c(leg.lty,"dotted");leg.col=c(pl.col[i],leg.col)
      pt.bg=c(pt.bg,NA)
    }
  }


     leg.txt=c(pred.name,leg.txt); leg.pch=c(pred.pch=NA,leg.pch); leg.lty=c(pred.lty=1,leg.lty);leg.col=c(pred.col="black",leg.col);pt.bg=c(pred.fill.col=NA,pt.bg)

  # Plot observed data
  if(!is.null(obs)) {
    if(!switch.xy) {
      if (!is.null(obs.pch)) {points(x,y=obs,pch=obs.pch,col=obs.col,bg=obs.fill.col)} else {obs.pch=NA}
      if (!is.null(obs.lty)) {lines(x,y=obs,lwd=obs.lwd,lty=obs.lty,col=obs.col)} else {obs.lty=NA}
    } else {
      if (!is.null(obs.pch)) {points(x=obs,y=x,pch=obs.pch,col=obs.col,bg=obs.fill.col)} else {obs.pch=NA}
      if (!is.null(obs.lty)) {lines(x=obs,y=x,lwd=obs.lwd,lty=obs.lty,col=obs.col)} else {obs.lty=NA}
    }
    leg.txt=c(obs.name,leg.txt); leg.pch=c(obs.pch,leg.pch); leg.lty=c(obs.lty,leg.lty);leg.col=c(obs.col,leg.col);pt.bg=c(obs.fill.col,pt.bg)
  }


  # Add legend
  if(leg.add) {
    if(all(is.na(leg.lty))) {
      legend(x=leg.x,legend=leg.txt,pch=leg.pch,col=leg.col,cex=leg.cex.text,pt.bg=pt.bg,bg="white",bty=leg.bty)
    } else {
      legend(x=leg.x,legend=leg.txt,pch=leg.pch,lty=leg.lty,col=leg.col,cex=leg.cex.text,pt.bg=pt.bg,bg="white",bty=leg.bty)
    }
  }
  if (add.indices) { # Add indices
    if (is.null(pred.reps)) {
      warning("Unable to add numerical indices because pred.reps=NULL in F:plot.problim")
    } else if (!is.null(obs) & !is.null(pred)) {
       nash=nse(obs=obs,pred=pred,na.rm=T)
       avg_obs=mean(obs,na.rm=T)
       avg_pred=mean(pred,na.rm=T)
       abs_error=avg_pred-avg_obs
       rel_error=(abs_error)/avg_obs*100
       mean.pred.reps.vec=apply(pred.reps,1,mean)
       sd.pred.reps.vec=apply(pred.reps,1,sd)
       pi.abs=mean(1/sd.pred.reps.vec)                # equation 25 in Renard et al., 2010
       pi.rel=mean(mean.pred.reps.vec/sd.pred.reps.vec)  # equation 26 in Renard et al., 2010
       average.sd=mean(sd.pred.reps.vec)
       average.CV=mean(sd.pred.reps.vec/mean.pred.reps.vec)
       mean.pred.reps.scal=mean(mean.pred.reps.vec)
       rel_error.pred.reps=(mean.pred.reps.scal-avg_obs)/avg_obs*100

       # Plot indices
       title(sub=bquote(paste(NSE==.(format(nash,digits=2)),",",
                              bar(obs)==.(format(avg_obs,scientific=T,digits=3)),",",
                              bar(pred)==.(format(avg_pred,scientific=T,digits=3)),",",
                              bar(pred.reps)==.(format(mean.pred.reps.scal,scientific=T,digits=3)),",",
                              abs.er==.(format(abs_error,scientific=T,digits=3)),",",
                              rel.er.pred==.(format(rel_error,digits=2)),",",
                              rel.er.reps==.(format(rel_error.pred.reps,digits=2)),",",
                              pi^abs==.(format(pi.abs,digits=2)),",",
                              pi^rel==.(format(pi.rel,digits=2)),",",
                              bar(sigma)==.(format(average.sd,digits=2)),",",
                              bar(CV)==.(format(average.CV,digits=2))
       )),cex.sub=0.5)

     } else {

       mean.pred.reps=apply(pred.reps,1,mean)
       sd.pred.reps=apply(pred.reps,1,sd)
       pi.abs=mean(1/sd.pred.reps)                # equation 25 in Renard et al., 2010
       pi.rel=mean(mean.pred.reps/sd.pred.reps)  # equation 26 in Renard et al., 2010
       average.sd=mean(sd.pred.reps)
       average.CV=mean(sd.pred.reps/mean.pred.reps)

       # Plot indices
       title(sub=bquote(paste(pi^abs==.(format(pi.abs,digits=2)),", ",
                              pi^rel==.(format(pi.rel,digits=2)),", ",
                              bar(sigma)==.(format(average.sd,digits=2)),", ",
                              bar(CV)==.(format(average.CV,digits=2))
       )),cex.sub=0.75)


     }
     ##note<< Numerical indices \code{pi.abs} and \code{pi.rel} are from Eq. (25) and Eq. (26) in \href{http://dx.doi.org/10.1029/2009WR008328}{Renard et al(2010)}
     ##note<< Numerical indices \code{bad(sigma)} and \code{bad(CV)} are the expected values of the standard deviation and the coefficient of variation from all time series
  }

  return(invisible(pred.pl))

}

calc.problim<-function(
### Calculate probability limits from a matrix of simulated replicates
##title<< Calculate probability limits
##details<< If \code{pl.type="ts"} the probability limits (pl) are calc. directly on the pred.reps. If \code{pl.type="prob"} then each replicate of pred.reps is sorted prior to calculating the pl,
## this second option is useful when calculating pl of prob. distributtions
## The \code{pl} argument provides a vector of probabilities that are used to determine the percentiles, which determine the probability limits.
## For example, \code{pl=c(0.5,0.9}, refers to the 50% and 90% probability limits, which means the percentiles will be 5%,25%,75% and 95%, and this will be used to calculate the pl
## of \code{pred.reps}
pred.reps, ##<< replicates of prediction, in matrix form: \code{[ndata,nreps]} where \code{ndata} is the number of data points and \code{nreps} is the number of replicates
percentiles=c(0.05,0.5,0.95), ##<< percentiles at which to calculate probability limits, default = \code{c(0.05,0.5,0.95)}
pl=NULL, ##<< probabilty to be used for probability limit, see details
pl.type=c("ts","prob") ##<< type of probability limits (pl), two possible options: for \code{"ts"} (time series), or \code{"prob"}, see details,
){
      # Calculates Probability limits for a series of replicates of predictions
      # Assumes pred.reps is a matrix (nData,nRep)
      # Outputs a matrix, nrow = nData, and each column, each quantile

      pl.type=match.arg(pl.type)
      if(!is.null(pl)) percentiles=calc.percentiles.from.pl(pl)
      # Sort columns first if doing a pl.type = probability
      if (pl.type=="prob") {
        if(any(is.na(pred.reps))) {
            browser()
            warning("NA's exist in pred.reps, as pl.type = 'prob' pred.reps are first sorted, before calc. pl. During sorting, NA's will be last")
        }
        pred.reps=apply(pred.reps,MARGIN=2,FUN=sort,na.last=T)
      }

      # Get probability limits
      pl.series=t(apply(pred.reps,MARGIN=1,FUN=quantile,percentiles,na.rm=T))

      return(pl.series)
      ### Matrix of probability limits
}

calc.percentiles.from.pl=function(
### Calculate percentiles from a set of probability limits
  pl ##<< probability limits
  ){
  pl=sort(pl,decreasing=T)
  n.pl=length(pl)
  n.percentiles=2*n.pl
  percentiles=vector(length=n.percentiles)
  for (i in 1:length(pl)){
    percentiles[i]=0.5-pl[i]/2
    percentiles[n.percentiles-i+1]=0.5+pl[i]/2
  }
  return(percentiles)
  ### returns percentiles
}




