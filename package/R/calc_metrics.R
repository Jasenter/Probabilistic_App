

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


sharpness = function(pred.reps,pred.reps.base=NULL) {
  if (!is.null(pred.reps.base)){
    q_5_95_pred = apply(pred.reps,1,quantile,c(0.05,0.95),na.rm=T)
    q_5_95_base = apply(pred.reps.base,1,quantile,c(0.05,0.95),na.rm=T)
    if (length(q_5_95_pred[2,]-q_5_95_pred[1,])!=length(q_5_95_base[2,]-q_5_95_base[1,])){
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
  pred.reps.base = calc_ClimDaily_dayOfYearWindow_seamless(QobsCal=data[[opt$obs]],datesCal=data[[opt$date]])
  sharpness = sharpness(pred.reps=pred.reps,pred.reps.base=pred.reps.base)
  bias = abs_bias_prob(obs=data[[opt$obs]],pred=pred.reps)

  return(list(reliability=reliability,sharpness=sharpness,bias=bias))
}


