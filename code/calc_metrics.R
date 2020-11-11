sa_nse = function(obs,pred,dates,seasons='months'){
  
  if (seasons=='months'){
    nEvalPeriod = 12
    select.fmt = '%m'
  }
  
  meanObsMon = c()
  for (m in 1:nEvalPeriod){
    keep = as.integer(format(dates,select.fmt))==m
    meanObsMon[m] = mean(obs[keep],na.rm=T)
  }
  month_on_day = as.integer(format(dates,select.fmt))
    
  obs = obs - meanObsMon[month_on_day]
  pred = pred - meanObsMon[month_on_day]
    
  SA_NSE = nse(obs=obs,pred=pred)
  
  return(SA_NSE)
  
}

################

PQQ_alpha = function(obs,pred.reps,perturb=F){
  
  if (perturb){
    nObs = dim(pred.reps)[1]
    nReps = dim(pred.reps)[2]
    pred.reps = pred.reps + matrix(runif(length(pred.reps),0,1e-8),ncol=nReps)
    obs = obs + runif(nObs,0,1e-8)
  }

  obs.pval = calc.obs.pval(obs=obs,pred.reps=pred.reps)
  pval_sort = sort(obs.pval, index.return = T)
  y = pval_sort$x
  n = length(y)
  i = seq(1:n)
  PP = i/(n + 1)
  plotpoints = qqplot(PP, y, plot.it = F)
  alpha.prime = mean(abs(plotpoints$y - plotpoints$x))
  alpha = 1 - 2 * alpha.prime
  
  return(1.-alpha)
  
}

################

ave_CV = function(pred.reps,pred.mode){
  
  sd.pred.reps = apply(pred.reps,1,sd,na.rm=T)
  mean.pred.reps = apply(pred.reps,1,mean)
  CV = sd.pred.reps/mean.pred.reps
  average.CV = mean(CV)
  
  return(average.CV)
  
}

################

prec_mean_obs = function(pred.reps,obs){
  
  sd.pred.reps = apply(pred.reps,1,sd,na.rm=T)
  mean.obs = mean(obs,na.rm=T)
  prec_mean_obs = mean(sd.pred.reps,na.rm=T)/mean.obs

  return(prec_mean_obs)
  
}

################

bias_pct =  function(obs,pred){
  
  return(mean(pred-obs)/mean(obs)*100.)
  
}

################

bias_prob =  function(obs,pred){
  
  return((mean(pred)-mean(obs))/mean(obs))
  
}

################

abs_bias_prob =  function(obs,pred){
  mean.obs = mean(obs,na.rm=T)
  mean.pred = mean(pred,na.rm=T)
  bias = abs((mean.pred-mean.obs)/mean.obs)
  
  return(bias)
  
}
################

calc_metrics = function(data,pred.reps,opt){
  reliability = PQQ_alpha(obs=data[[opt$obs]],pred.reps=pred.reps,perturb=T)
  #precision = prec_mean_obs(pred.reps=pred.reps,obs=data$obs)
  pred.reps.base = calc_ClimDaily_dayOfYearWindow_seamless(QobsCal=data[[opt$obs]],datesCal=data[[opt$date]])
  sharpness = sharpness(pred.reps=pred.reps,pred.reps.base=pred.reps.base)
  bias = abs_bias_prob(obs=data[[opt$obs]],pred=pred.reps)
  
  return(list(reliability=reliability,sharpness=sharpness,bias=bias))
}

################
# Calculate prob limits: 10%, 25%, 50%, 75%, 90%
# calc_problimit = function(obs,pred.reps,reps){
#   n = length(obs)
#   prob.limit = list(n,n,n,n,n,n)
#   prob.limit[[1]] = obs
#   for (i in 1:n) {
#     prob.limit[[2]][i] = sort(pred.reps[i,])[reps*0.05]
#     prob.limit[[3]][i] = sort(pred.reps[i,])[reps*0.25]
#     prob.limit[[4]][i] = sort(pred.reps[i,])[reps*0.5]
#     prob.limit[[5]][i] = sort(pred.reps[i,])[reps*0.75]
#     prob.limit[[6]][i] = sort(pred.reps[i,])[reps*0.95]
#   }
#   #browser()
#   
#   names(prob.limit) = c("obs","fifth percentile","twenty-fifth percentile","median","seventy-fifth percentile","ninety-fifth percentile")
#   
#   return(prob.limit)
#}
###############

sharpness = function(pred.reps,pred.reps.base=NULL) {
  if (!is.null(pred.reps.base)){
    q_5_95_pred = apply(pred.reps,1,quantile,c(0.05,0.95),na.rm=T)
    q_5_95_base = apply(pred.reps.base,1,quantile,c(0.05,0.95),na.rm=T)
    if (length(q_5_95_pred[2,]-q_5_95_pred[1,])!=length(q_5_95_base[2,]-q_5_95_base[1,])){
      #print(length(q_5_95_pred[2,]-q_5_95_pred[1,]))
      #print(length(q_5_95_base[2,]-q_5_95_base[1,]))
      # this is used to catch cases where scen has monthly data (e.g. monthly QPP), and scenBase is daily
      IQR90 = NA
      IQR90_mod = NA
    } else {
      IQR90 = mean((q_5_95_pred[2,]-q_5_95_pred[1,])/(q_5_95_base[2,]-q_5_95_base[1,]),na.rm=T)
      IQR90_mod = mean(q_5_95_pred[2,]-q_5_95_pred[1,],na.rm=T)/mean(q_5_95_base[2,]-q_5_95_base[1,],na.rm=T)
    }  
  } else {
    IQR90 = NA
    IQR90_mod = NA
  }
  return(IQR90_mod)
}
