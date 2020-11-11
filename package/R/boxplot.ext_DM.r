boxplot.ext=function(xin,whiskersProb=c(0.025,0.975),colouring,at=NULL,...){
# Draws a boxplot with the whiskers at the probability limits, provided by whiskersProb
# x can be vector or data.frame
# whiskers Prob values are probabilities for the lower and upper whisker (respectively)

x = data.frame(xin)  
x.stats=boxplot(x,plot=FALSE)
#browser()
if ( is.data.frame(x) || is.matrix(x)) {
  x.stats$out = c()
  x.stats$group = c()
  y=x
  for (j in 1:ncol(x)) {
    y=sort(x[,j])
    if (length(na.omit(y))!=0) {
      x.stats$stats[1,j]=quantile(y,prob=whiskersProb[1])
      x.stats$stats[5,j]=quantile(y,prob=whiskersProb[2])
    }
    for (i in 1:length(na.omit(y))){
      if (y[i]<x.stats$stats[1,j]|y[i]>x.stats$stats[5,j]){
 #       browser()
        x.stats$out = c(x.stats$out,y[i])
        x.stats$group = c(x.stats$group,j)
#        browser()
      }
    }
#    browser()
  }
} else if (is.vector(x)) { # Assume x is a vector
  x.stats$out = c()
  x.stats$group = c()
  y=sort(x)
  if (length(na.omit(y))!=0) {
    x.stats$stats[1,1]=quantile(y,prob=whiskersProb[1])
    x.stats$stats[5,1]=quantile(y,prob=whiskersProb[2])
    }
    for (i in 1:length(na.omit(y))){
      if (y[i]<x.stats$stats[1]|y[i]>x.stats$stats[5]){
      #        browser()
      x.stats$out = c(x.stats$out,y[i])
      x.stats$group = c(x.stats$group,1)
      #        browser()
      }
    }
} else
{
  print("type of x is not supported in boxplot.ext")
  return()
}

#browser()

#bxp(z=x.stats,...)
bxp(z=x.stats,at=at,boxfill=colouring,...)
#shaded.bxp(z=x.stats,...)

}
attr(boxplot.ext,"ex") <- function(){

 # Examples
x=rnorm(1e6);par(ask=T)
#Test whether it works for a vector - default is 95% probability limits
boxplot.ext(x,ylim=c(-3,3))
# Test whether it works for a data.frame - default is 95% probability limits
x=data.frame(x1=x,x2=x,x3=x)
boxplot.ext(x,ylim=c(-3,3))
# Draw 90% Probability Limits
boxplot.ext(x,whiskersProb=c(0.05,0.95))
# # Uneven probability limts
boxplot.ext(x,whiskersProb=c(0.20,0.99))
# Add colour
boxplot.ext(x,whiskersProb=c(0.20,0.99),boxfill="blue")
# 
# #
# #Generate some parameter samples for different scenarios, "tom", "dick" and "harry"
tom=rnorm(n=1000,mean=1000,sd=50)
 dick=rnorm(n=1000,mean=2000,sd=200)
 harry=rnorm(n=1000,mean=1500.8,sd=100)
# # Enter them into a data.frame 
par.samps=data.frame(tom,dick,harry)
# 
# # Plot them up
boxplot.ext(par.samps,horizontal=T,las=1,outline=F,xlab="Parameter Name")
# 

}

