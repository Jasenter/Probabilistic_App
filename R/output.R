# Designs the front page of the output pdf
# Ranking the catchment's metrics wrt HRS metrics
# HRS data are the medians and the upper & lower (66th &33rd respectively) quantiles
# quantiles are taken from the linear mean, NSE objective function, BC02 error model

#######################################
## ranking the catchment wrt HRS

rankHRS = function(metrics,dir.loc) {
  # HRS catchment rankings
  good = c(0.056, 0.364, 0.033) # 25th quantiles
  ok.med = c(0.080, 0.528, 0.091) # medians
  bad = c(0.129, 0.792, 0.256) # 75th quantiles
  metric.name = c("reliability","sharpness","bias")

  rank = vector(length=3)
  rankVector = vector(length=3)
  rankVector.text = vector(length=3)
  met = c(metrics[[1]],metrics[[2]],metrics[[3]])

  for (i in 1:3) {
    if(metrics[[i]] < good[i]){
      rank[i] = "good"
    } else if (metrics[[i]] > bad[i]) {
      rank[i] = "bad"
    } else {
      rank[i] = "ok"
    }

    # Calling HRS data to rank by percentile
    RData_fname = paste(dir.loc,"/",metric.name[i],'.RData',sep='')
    load(RData_fname)
    HRSlab = paste("HRS",metric.name[i],sep="")
    rankVector[i] = round(match(metrics[[i]],sort(c(metrics[[i]],get(HRSlab)[[1]])))/length(c(metrics[[i]],get(HRSlab)[[1]])),digits=3)*100
    rankVector.text[i] = paste(rankVector[i],"%",sep="")

    met[i] = round(metrics[[i]],digits=3) # round now that the calculations with it are done
  }
  met.table = data.frame(met,rankVector.text,rank)
  
  return(met.table)
}

#######################################
## Writing the parameters

# Writing parameters & metrics
output.main=function(param,metrics,msg.print=NA,data=NA,is.data=T,opt,dir.loc="") {
  layout(mat=matrix(c(1,1,
                      2,3,
                      4,4),
                    nrow=3,ncol=2,byrow=T))

# Header
  plot(NA,xlim=c(1,10),ylim=c(1,4),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
  text(x=5,y=3,label=c("Probabilistic Predictions"),cex=1.5,font=2)
  text(x=5,y=2,label=c(paste("Designed by the UofA Water Group 2020",
                            "Please direct all comments and bug reports to jason.hunter@adelaide.edu.au",sep="\n")))
# Metrics
  met.table = rankHRS(metrics=metrics,dir.loc=dir.loc)
  plot(NA,xlim=c(0.5,3.5),ylim=c(0.5,3.5),xaxt="n",yaxt="n",xlab="",ylab="",main="Metrics")
  text(x=0.5,y=c(3:1),label=c("Reliability","Sharpness","Bias"),pos=4)
  text(x=2,y=c(3:1),label=met.table[[1]])
  text(x=2.75,y=c(3:1),label=met.table[[2]],pos=4)
  abline(v=(1.5:2.5))
  abline(h=(1.5:2.5))

# Parameters
  param.round = c(round(param$A,3),round(param$lambda,3),round(param$mean_eta_0,3),round(param$mean_eta_1,3),round(param$rho,3),round(param$sigma_y,3))
  plot(NA,xlim=c(1,2),ylim=c(0.75,6.25),xaxt="n",yaxt="n",xlab="",ylab="",main="Parameters")
  text(x=1,y=c(6:1),label=c("Offset (A)","Lambda","Mean intercept (alpha)","Mean slope (beta)","Lag-1 AR coeff. (phi)","Innovation sd. (sigma)"),pos=4)
  text(x=1.75,y=c(6:1),label=param.round)
  abline(v=(1.5:2.5))
  abline(h=(1.5:5.5))

# Errors
  if (is.data==T) {
    msg.print=error.print(data,opt) # checks the input data for problems and writes the vector of error messages
  }
  plot(NA,xlim=c(1,3),ylim=c(0.5,5.5),xaxt="n",yaxt="n",xlab="",ylab="",main="Error check diagnostics")
  text(x=1,y=(5:1),label=msg.print[1:5],pos=4)

}





