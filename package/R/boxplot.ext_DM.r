boxplot.ext=function(xin,whiskersProb=c(0.025,0.975),colouring,at=NULL,...){
# Draws a boxplot with the whiskers at the probability limits, provided by whiskersProb
# x can be vector or data.frame
# whiskers Prob values are probabilities for the lower and upper whisker (respectively)

x = data.frame(xin)
x.stats=boxplot(x,plot=FALSE)

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

        x.stats$out = c(x.stats$out,y[i])
        x.stats$group = c(x.stats$group,j)

      }
    }

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

      x.stats$out = c(x.stats$out,y[i])
      x.stats$group = c(x.stats$group,1)

      }
    }
} else
{
  print("type of x is not supported in boxplot.ext")
  return()
}

bxp(z=x.stats,at=at,boxfill=colouring,...)

}

