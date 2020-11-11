### Ranking the catchment's metrics wrt HRS metrics
# HRS data  These are the medians and the upper & lower (66th &33rd respectively) quantiles
# quantiles taken from the linear mean, NSE objective function, BC02 error model
rankHRS = function(metrics) {
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
    RData_fname = paste(data_dirname,metric.name[i],'.RData',sep='')
    load(RData_fname)
    HRSlab = paste("HRS",metric.name[i],sep="")
    rankVector[i] = round(match(metrics[[i]],sort(c(metrics[[i]],get(HRSlab)[[1]])))/length(c(metrics[[i]],get(HRSlab)[[1]])),digits=3)*100
    #rankVector[i] = round(rankVector[i],digits=3)*100
    rankVector.text[i] = paste(rankVector[i],"%",sep="")

    met[i] = round(metrics[[i]],digits=3) # round now that the calculations with it are done
  }
  met.table = data.frame(met,rankVector.text,rank)
  return(met.table)
}
##############################################################


##############################################################
## Writing page 1 (front page)

frontpage = function(inputName,param,metrics){

  met.table.param = data.frame(round(param$A,3),round(param$lambda,3),round(param$mean_eta_0,3),round(param$mean_eta_1,3),round(param$rho,3),round(param$sigma_y,3))

  met.table = rankHRS(metrics)

  header.A = textGrob("Probabilistic Predictions",just="bottom",gp=gpar(fontsize=20))
  header.B = textGrob(label=paste("Designed by the UofA Water Group (Bennett, Thyer, McInerney, Hunter, Kavetski, Leonard et al.) 2020",
                                  "Please direct all comments and bug reports to jason.hunter@adelaide.edu.au",sep="\n"),
                      #                       just = "right",
                      gp = gpar(col="grey",fontsize=10))
  header.C = textGrob(label=paste('Predictive plots and values generated from input data "',inputName,'"',sep=""),
                      gp=gpar(col="black",fontsize=10),
                      just=c("centre","bottom"))
  header.D = textGrob(label="Metrics",gp=gpar(fontsize=15),hjust=4,vjust=1)
  header.E = textGrob(label="Parameters",gp=gpar(fontsize=15),hjust=3,vjust=3)
  spacer = textGrob(label="   ")

  x = tableGrob(met.table,
                rows=c("Reliability","Sharpness","Bias"),
                cols=c("input_data_metrics","input_data_percentile","input_data_classification"))

  y = tableGrob(met.table.param,
                rows=c(" "),
                cols=c("Offset","Lambda","Intercept","Slope","Rho","Sigma_y"))

  grid.arrange(header.A,header.B,header.C, spacer, header.D,x, header.E,y, ncol=1, nrow=8,heights=c(3,1,6,6,1,8,1,8))
  return()
}
##############################################################


##############################################################
## Writing page 2 (error messages)

error.write = function(msg.print=NA,data=NA,opt=NA,is.data=T) {

  if (is.data==T) {
    msg.print=error.print(data=data,opt=opt) # checks the input data for problems and writes the vector of error messages
  }


  header.error = textGrob("Output messages",gp=gpar(fontsize=20))
  error.A = textGrob(msg.print[1])
  error.B = textGrob(msg.print[2])
  error.C = textGrob(msg.print[3])
  error.D = textGrob(msg.print[4])
  error.E = textGrob(msg.print[5])
  grid.arrange(header.error,error.A,error.B,error.C,error.D,error.E,ncol=1,nrow=length(msg.print)+1)

  return()
}


