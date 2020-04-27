#np.y.cond.x=function(
np.sdy.cond.x = function(
  ### Uses non-parametric estimation techniques to estimate the sd of \code{y} as a function of \code{x}
  ##title<<  Non-parameter sd estimation of y conditional on \code{x} 
  ##details<< Uses adaptive bandwidth to ensure the sd of y is estimated using a regular spaced series of values of x. The spacing is calculated such that the same number of points of x is used to estimate the sd of y. This ensures that the estimates of sd estimates have the same uncertainty (which is currently not calculated)  
  y, ##<< vector of \code{y}
  x, ##<< vector of \code{x}
  prop.x=0.05, ##<< proportion of points in \code{x} used to calculate regular spacing of sd[y] estimates, default is \code{0.05}.
  bandwidth.ratio=1 ##<< ratio of the spacing used to calculate the bandwidth used in the kernel density estimation, default is \code{1} which means no overlap in bandwidth between sd estimates. 
)
  
{
  # Firstly sort x as an increasing function
  sorted.x=sort(x,index.return=T)
  x=sorted.x$x
  # Then first y is also sorted in same order 
  y=y[sorted.x$ix]
  
  # Calculate sequence points where the sd will be calculated.
  # Points where sd will be calculated represent midpoints between break points
  # Breakpoints, represent the start and end of the points in x used to calculate sd[y]
  equal.p.breakpts=seq(0,1,by=prop.x)
  breakpts=quantile(x,probs=equal.p.breakpts,type=5,names=F)
  n.breakpts=length(breakpts)
  midpts=(breakpts[2:n.breakpts]+breakpts[1:(n.breakpts-1)])/2
  
  equal.p.midpts=seq(0.5*prop.x,1-(0.5*prop.x),by=prop.x)
  
  #Calculate sequence points where the sd will be calculated is called x.seq
  x.seq=midpts
  n.x.seq=length(x.seq)
  #Adaptive bandwith
  bandwidth=bandwidth.ratio*breakpts[2:length(breakpts)]-breakpts[1:(length(breakpts)-1)]
  y.seq=vector(mode = "numeric", length = n.x.seq)   # vector of y at x.seq 
  
  # kernel estimation
  for (j in 1:n.x.seq){
    # Use adaptive bandwidth, based on a minimum number of percentage of points within bandwidth
    x.kernel=(x-x.seq[j])/bandwidth[j] # Calculate the kernel  
    w.x=(3/4)*(1-x.kernel^2)*(abs(x.kernel)<1) # epanechnikov kernel
    y.seq[j]=sum(y*w.x,na.rm=T)/sum(w.x)  # y - see equation (5)
  }
  
  return (list(x.seq=x.seq,y.seq=y.seq,bandwidth=bandwidth))
}

################


np.moments.cond.x=function(
  ### Uses non-parametric estimation techniques to estimate the sd of \code{y} as a function of \code{x}
  ##title<<  Non-parameter sd estimation of y conditional on \code{x} 
  ##details<< Uses adaptive bandwidth to ensure the sd of y is estimated using a regular spaced series of values of x. The spacing is calculated such that the same number of points of x is used to estimate the sd of y. This ensures that the estimates of sd estimates have the same uncertainty (which is currently not calculated)  
  y, ##<< vector of \code{y}
  x, ##<< vector of \code{x}
  prop.x=0.05, ##<< proportion of points in \code{x} used to calculate regular spacing of sd[y] estimates, default is \code{0.05}.
  bandwidth.ratio=1 ##<< ratio of the spacing used to calculate the bandwidth used in the kernel density estimation, default is \code{1} which means no overlap in bandwidth between sd estimates. 
)
  
{
  # Firstly sort x as an increasing function
  sorted.x=sort(x,index.return=T)
  x=sorted.x$x
  # Then first y is also sorted in same order 
  y=y[sorted.x$ix]
  
  # Calculate sequence points where the sd will be calculated.
  # Points where sd will be calculated represent midpoints between break points
  # Breakpoints, represent the start and end of the points in x used to calculate sd[y]
  equal.p.breakpts=seq(0,1,by=prop.x)
  breakpts=quantile(x,probs=equal.p.breakpts,type=5,names=F)
  n.breakpts=length(breakpts)
  midpts=(breakpts[2:n.breakpts]+breakpts[1:(n.breakpts-1)])/2
  
  equal.p.midpts=seq(0.5*prop.x,1-(0.5*prop.x),by=prop.x)
  
  #Calculate sequence points where the sd will be calculated is called x.seq
  x.seq=midpts
  n.sd=length(x.seq)
  #Adaptive bandwith
  bandwidth=bandwidth.ratio*breakpts[2:length(breakpts)]-breakpts[1:(length(breakpts)-1)]
  var.y=vector(mode = "numeric", length = n.sd)   # vector of the variances of y
  skew.y=vector(mode = "numeric", length = n.sd)   # vector of the variances of y
  mean.y=vector(mode = "numeric", length = n.sd)   # vector of the variances of y
  sd.y = vector(mode="numeric",length=n.sd)
  kurt.y = vector(mode="numeric",length=n.sd)
  n = length(y)
  bandsize = n/n.sd
  nullquence = seq(from=0,to=n.sd,by=1)
  # kernel estimation
  for (j in 1:n.sd){
    start = (nullquence[j]*bandsize) + 1
    end = bandsize*j
    # Use adaptive bandwidth, based on a minimum number of percentage of points within bandwidth
    x.kernel=(x-x.seq[j])/bandwidth[j] # Calculate the kernel  
    #w.x=(3/4)*(1-x.kernel^2)*(abs(x.kernel)<1) # epanechnikov kernel
    #var.y[j]=sum(y^2*w.x,na.rm=T)/sum(w.x)  # variance - see equation (5)
    var.y[j] = ((sum(y[start:end]-mean(y[start:end])))^2)/bandsize
    #skew.y[j]=sum(y^3*w.x,na.rm=T)/sum(w.x)  # skew 
    mean.y[j] = mean(y[start:end])
    #mean.y[j]=sum(y*w.x,na.rm=T)/sum(w.x)  # mean 
    sd.y[j] = sd(y[start:end])
    skew.y[j] = skewness(y[start:end])
    kurt.y[j] = kurtosis(y[start:end])-3
  }

  #skew.y=42
  #skew.y = skew.y/var.y^(3/2)
  #print("andhere?",mean.y)
  return (list(x=x.seq,sd.y=sd.y,skew.y=skew.y,mean.y=mean.y,skew.y=skew.y,kurt.y=kurt.y,bandwidth=bandwidth))
}
################
plot.np.sdy.cond.x=function(
  ### Plots non-parametric estimation of sd of \code{y} as a function of \code{x},
  ##title<<  Plot non-parameter sd estimation of y conditional on \code{x} 
  ##details<< see \code{\link{np.sdy.cond.x}} for further information, TODO: add ability to specify 
  
  metFlag,
  y, ##<< vector of \code{y}
  x, ##<< vector of \code{x}
  prop.x=0.05, ##<< proportion of points in \code{x} used to calculate regular spacing of sd[y] estimates, default is \code{0.05}.
  bandwidth.ratio=1, ##<< ratio of the spacing used to calculate the bandwidth used in the kernel density estimation, default is \code{1} which means no overlap in bandwidth between sd estimates. 
  sd.y.col="cyan", ##<< colour of line for sd[y] as function of x
  sd.y.pch=13, ##<< symbol used on line of sd[y] as function of x
  sd.y.lwd=5,
  sd.y.lty=1,
  skew.y.col="green", ##<< colour of line for sd[y] as function of x
  skew.y.pch=13, ##<< symbol used on line of sd[y] as function of x
  skew.y.lwd=5,
  skew.y.lty=1,
  mean.y.col="red", ##<< colour of line for sd[y] as function of x
  mean.y.pch=13, ##<< symbol used on line of sd[y] as function of x
  mean.y.lwd=5,
  mean.y.lty=1, 
  kurt.y.col="gold",
  kurt.y.pch=13,
  kurt.y.lwd=5,
  kurt.y.lty=1,
  lm.lwd=5,
  lm.y.lty=1,
  lm.y.pch = 13,
  lm.y.col="purple",
  add.leg=F, ##<< add a legend
  xlab="x", ##<< names of x, used in legend
  ylab="y", ##<< names of y, used in legend
  add.pts=T, ##<< Do you want to add the pts to the plot
  add=F, ##<< Do you want to add to an existing plot
  add.CI=F, ##<< Do you want to confidence limits
  simy.func=rnorm, ##<< function used to simulate y values, for plotting confidence limits
  ... ##<< additional arguments passed to \code{\link{plot.default}}
)
  
{
  # First calculate sd estimates of y as a function of x
  est.moments=np.moments.cond.x(x=x,y=y,prop.x=prop.x,bandwidth.ratio=bandwidth.ratio)
  
  # First set-up plot
  if (!add) {
    plot(x=x,y=y,type="n",xlab=xlab,ylab=ylab,...)
  }
  
  # Plot points if required
  if (add.pts) {
    points(x=x,y=y,...)
  }
  
  if(isTRUE(metFlag[1])){
    # Undertake plotting, plot mean[y] as function of x
    lines(x=est.moments$x,y=est.moments$mean.y,type="l",col=mean.y.col,pch=mean.y.pch,
          lty=mean.y.lty,lwd=mean.y.lwd)
  }
  if(isTRUE(metFlag[2])){
  # Undertake plotting, plot sd[y] as function of x
  lines(x=est.moments$x,y=est.moments$sd.y,type="l",col=sd.y.col,pch=sd.y.pch,
        lty=sd.y.lty,lwd=sd.y.lwd)
  }
  if(isTRUE(metFlag[3])) {
    # Undertake plotting, plot skew[y] as function of x
    lines(x=est.moments$x,y=est.moments$skew.y,type="l",col=skew.y.col,pch=skew.y.pch,
          lty=skew.y.lty,lwd=skew.y.lwd)
  }
  if(isTRUE(metFlag[4])) {
    # Undertake plotting, plot skew[y] as function of x
    lines(x=est.moments$x,y=est.moments$kurt.y,type="l",col=kurt.y.col,pch=kurt.y.pch,
          lty=kurt.y.lty,lwd=kurt.y.lwd)
  }
  if(isTRUE(metFlag[5])) {
    m = lm(y~x)
    mu0 = m$coefficients[1]
    mu1 = m$coefficients[2]
    abline(a=mu0,b=mu1,lwd=lm.lwd,col=lm.y.col)
  }

  if (add.CI) { # Add confidence intervals, by simulation
    
    n.reps=1e3
    n.y=length(y)
    n.sims=n.y*n.reps
    sim.y=simy.func(n=n.sims) # Generate simulations
    sim.y.mat=matrix(data=sim.y,nrow=n.y,ncol=n.reps) # Convert to matrix
    sd.sim.y.mat=matrix(data=NA,nrow=1/prop.x,ncol=n.reps) # Convert to matrix
    for (i in 1:n.reps) {
      sd.sim.y=np.sdy.cond.x(y=sim.y.mat[,i],x=x,prop.x=prop.x,bandwidth.ratio=bandwidth.ratio) # Calculate sd.sim.y
      sd.sim.y.mat[,i]=sd.sim.y$sd.y
    }
    # Calc probability limits
    sd.sim.y.pl=calc.problim(pred.reps=sd.sim.y.mat,pl=0.9,pl.type="ts")
    means=colMeans(sd.sim.y.pl)
    sd.sim.y.pl[,1]=means[1]
    sd.sim.y.pl[,2]=means[2]
    lines(sd.sim.y$x,sd.sim.y.pl[,1],pch=NA,lty="dashed",col="black",...)
    lines(sd.sim.y$x,sd.sim.y.pl[,2],pch=NA,lty="dashed",col="black",...)
      
  }
  
  # Add legend
  if(add.leg){
    legend(x="topleft",legend=c(ylab,paste("SD[",ylab,"]",sep="")),pch=c(1,sd.y.pch),col=c("black",sd.y.col),lty=c(NA,"solid"))
  }  
  
  # Return list of sd[y] conditional x.
  return(invisible(est.moments))
}

# Examples
attr(plot.np.sdy.cond.x,"ex") <- function(){

# Examples here all plot so-called set of residauls, against predictions, since this is the motivation of this code

# Generate synthetic model predictions
pred=runif(n=500)
# Generate some synthetic random normal "residuals"
residuals=rnorm(n=500) ;
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals")

# Generate some synthetic random normal "residuals" - no pts on plot
residuals=rnorm(n=500) ;
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals",add.pts=FALSE,add.leg=FALSE)

# add another line to the plot
residuals=rnorm(n=500,sd=2) ;
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals",add.pts=FALSE,add.CI=TRUE,add=TRUE,sd.y.col="blue",add.leg=FALSE)


# Try increasing bandwidth to show impact of smoothing
residuals=rnorm(n=1000) ;
pred=runif(n=1000)
par(mfrow=c(2,2))
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals",bandwidth.ratio=1.1,main="bandwidth.ratio=1.1")
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals",bandwidth.ratio=1.2,main="bandwidth.ratio=1.2")
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals",bandwidth.ratio=1.3,main="bandwidth.ratio=1.3")
plot.np.sdy.cond.x(x=pred,y=residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals",bandwidth.ratio=1.5,main="bandwidth.ratio=1.5")

# Generate some synthetic lognormal "predictions", to test adaptive bandwidth ability of np.sdy.cond.x
pred=rlnorm(n=1000)
par(mfrow=c(1,1))
plot.np.sdy.cond.x(x=pred,y=residuals,xlim=c(0,10),ylim=c(-3,3),xlab="predictions",ylab="residuals")

# Increase bandwdith to show effect of smoothing
plot.np.sdy.cond.x(x=pred,y=residuals,xlim=c(0,10),ylim=c(-3,3),xlab="predictions",ylab="residuals",bandwidth.ratio=1.5,main="bandwidth.ratio=1.5")

# Generate some synthetic heteroscedastic residuals, sd.residuals=a+b*pred, and uniform predictions
pred=runif(n=1000)
a=0.01; b=0.5 # Modify to test different scenarios

residuals=vector(mode = "numeric", length = length(pred))
for (i in 1:length(pred)) {
  sd=a+b*pred[i]
  residuals[i]=rnorm(n=1,sd=sd)
} 
# Standardize residuals, so that unconditional sd[residuals]=1
sd=sd(residuals)
std.residuals=vector(mode = "numeric", length = length(pred))
std.residuals=residuals/sd
plot.np.sdy.cond.x(x=pred,y=std.residuals,xlim=c(0,1),ylim=c(-3,3),xlab="predictions",ylab="residuals")

# Generate some synthetic heteroscedastic residuals, sd.residuals=a+b*pred, and lognormal predictions
pred=rlnorm(n=1000)
a=0.1; b=0.2 # # Modify to test different scenarios

residuals=vector(mode = "numeric", length = length(pred))
for (i in 1:length(pred)) {
  sd=a+b*pred[i]
  residuals[i]=rnorm(n=1,sd=sd)
} 
# Standardize residuals, so that unconditional sd[residuals]=1
sd=sd(residuals)
std.residuals=vector(mode = "numeric", length = length(pred))
std.residuals=residuals/sd
res=plot.np.sdy.cond.x(x=pred,y=std.residuals,ylim=c(-3,3),xlab="predictions",ylab="residuals")

}



