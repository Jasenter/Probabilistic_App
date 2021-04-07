############################################################################

plot.performance = function(data,pred.reps,type,opt,...){
  if (type == 'PQQ'){
    plot.predictiveQQ(obs=data[[opt$obs]],pred.reps=pred.reps,perturb=T,add.indices=F,xaxs='i',yaxs='i',...)
  }
}

############################################################################

plot.residuals = function(data,std.resids,type,model,param,opt,xlab=NULL,ylab=NULL,...){
  if (type == 'prob(pred)'){
    if (is.null(ylab)){ylab='Standardized residuals'}
    plot.residuals.multi(residuals=std.resids,pred=data[[opt$pred]],
                         plot.type=type, resid.outlier.thres = c(NA),
                         ylab=ylab,xaxs='i',yaxs='i',...)
    } else if (type == 'pred'){
      if (is.null(ylab)){ylab='Standardized residuals'}
      plot.residuals.multi(residuals=std.resids,pred=data[[opt$pred]],
                           plot.type=type, resid.outlier.thres = c(NA),
                           xlab=c(paste("Predictions (",opt$unit,")",sep="")),ylab=ylab,xaxs='i',yaxs='i',...)

    } else if (type == 'density'){
      if (is.null(xlab)){ylab='Standardized residuals'}
      if (is.null(ylab)){ylab='Probability density'}
      plot.residuals.multi(residuals=std.resids,pred=data[[opt$pred]],
                         plot.type=type, resid.outlier.thres = c(NA),
                         xlab=xlab,xaxs='i',ylab=ylab,...)

    } else if (type == 'tranz') {
      metFlag=c(F,F,F,F,F)
      tranzplotter(data=data,param=param,metFlag=metFlag,heteroModel=model,opt=opt)

    } else if (type == 'extratranz') {
      metFlag=c(T,T,T,T,T)
      tranzplotter(data=data,param=param,metFlag=metFlag,heteroModel=model,add.legend=T,opt=opt)

    } else if (type == 'acf'){
      acfplotter(data=data,acfType="acf",param=param,heteroModel=model,opt)

    } else if (type == 'pacf'){
      acfplotter(data=data,acfType="pacf",param=param,heteroModel=model,opt)
    }

}

############################################################################

boxplotter = function(data_dirname="",catchmentMetric,metric,boxColour) {
  #

  ## Opening Robject with HRS metrics
  RData_fname = paste(data_dirname,"/",metric,'.RData',sep='')
  load(RData_fname)
  HRSlab = paste("HRS",metric,sep="")

  # setting boxplot specifications
  ylim.max = max(sort(get(HRSlab)[[1]])[0.9*length(get(HRSlab)[[1]])],catchmentMetric)
  xrow = 0.41
  txtoffset = (ylim.max)*0.05
  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,4,4))
  # Plotting
  boxplot.ext(xin=get(HRSlab), ylim=c(0,ylim.max),
              colouring = boxColour,xaxt="n",ylab=c(""))

  # Adding points
  points(x=1,y=catchmentMetric,col="red",pch=4,cex=2,lwd=2)
  legend(x="topleft",legend=c("data metrics","HRS metrics"),pch=c(4,NA),pt.lwd=c(2,NA),col=c("red",boxColour),pt.cex=c(2,NA),
         fill=c(NA,boxColour),border=c(NA,"black"))
  mtext(side=3,line=1,text=c(metric),cex=2,font=2)
  mtext(side=2,line=3,font=2,cex=1.2, c("metric value"))
  axis(side=4,at=c(0.0,ylim.max),labels=c("better","worse"),las=2)
  #arrows(x0=xrow,x1=xrow,y0=0,y1=ylim.max,xpd=NA)
  #arrows(x0=xrow,x1=xrow,y0=ylim.max,y1=0,xpd=NA)
  #text(x=xrow,y=0-txtoffset,labels=c("better"),xpd=NA)
  #text(x=xrow,y=ylim.max+txtoffset,labels=c("worse"),xpd=NA)
  # legend(x="topright",legend=c(catchmentMetric))

}
############################################################################
############################################################################
timeseries = function(data,pred.reps,opt) {

  nlen = length(data[[opt$obs]])
  nt = floor(nlen/365) # numbers of timesteps in a year
  #final = length(data$obs-(nt*365)) # any remaining days
  for (i in 1:(nt+1)) {

    if (i > nt) { # last iteration
      if (nlen %% nt <= 0){ # catches when the data length divides perfectly into lengths of 365 days.
        return()
      }
      start = (nt*365)+1
      end = length(data[[opt$obs]])
    } else {
      start = (365*i)-364
      end = 365*i
    }

    ylim.max = max(data[[opt$obs]][start:end],sort(pred.reps[start:end,])[0.9*length(sort(pred.reps[start:end,]))],na.rm=T)
    plot.problim(obs=data[[opt$obs]],pred.reps=pred.reps,xlab='Time',ylab='Prediction (mmd)',add.indices=F,
                 xlim=c(start,end),ylim=c(0,ylim.max),xtype="date",date=data[[opt$date]],
                 pred=opt$pred,pred.lty=1,pred.lwd=2,pred.col="black",pred.name="Predicted")

    # axis(side=1,
    #      at=c(start,((end-start)/3)+start,(((end-start)/3)*2)+start,end),
    #      labels=c(data$date[start],data$date[((end-start)/3)+start],data$date[(((end-start)/3)*2)+start],data$date[end]))
    lines(data$pred,col="black",lwd=2)
  }
}
############################################################################
############################################################################
tranzplotter = function(data,param,metFlag=c(T,T,T,T,T),heteroModel,add.legend=F,add.title=F,opt) { # standardised residuals against transformed streamflow

  eta = calc_eta(Qobs=data[[opt$obs]],Qh=data[[opt$pred]],param=param,heteroModel=heteroModel) # transformed residuals
  eta.std = calc_std_resids(data,param,heteroModel=heteroModel,opt=opt) # standardised, transformed residuals
  sim = calc_tranz(Q=data[[opt$pred]],Qh=NULL,heteroModel=heteroModel,param=param) # transformed predicted flow

  plot.np.sdy.cond.x(x=sim,y=eta.std,ylab=c(""),xlab=c(""),metFlag=metFlag,prop.x=0.1)
  #axis(side=2,at=c(-1,0,1,2),labels=c("-1","0","1","2"))
  mtext(side=1,text=c("Transformed deterministic flow"),font=1,line=2)
  abline(h=0,col="grey",lty=2)
  mtext(side=2,text=expression(paste("Standardised residuals   ",bold(nu^"std"),sep=" ")),line=2,font=1)
  if(add.legend) {
    legend(x="topright",legend=c("mean","standard deviation","skewness","excess kurtosis","linear model"),col=c("red","cyan","green","gold","purple"),lwd=2:2,lty=1:1)
  }
  if(add.title) {
    mtext(side=3,line=2,text=c("Transformed residuals and flow with moving statistics"))
  }

}
############################################################################
############################################################################
acfplotter = function(data,acfType,param,heteroModel,opt) { # standardised residuals against transformed streamflow

  y=calc_innovations(data,param,heteroModel,opt)
  if (acfType == "acf"){
    acf(y,main="Lag analysis for residual innovations")
  } else if (acfType == "pacf") {
    pacf(y,main="Lag analysis for residual innovations")
  }

}

