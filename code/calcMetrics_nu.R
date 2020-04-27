PQQ_alpha = function(obs,pred.reps,perturb=F,obs.pval=NULL){ 

  if (is.null(obs.pval)){
    if (perturb){
      nObs = dim(pred.reps)[1]
      nReps = dim(pred.reps)[2]
      pred.reps = pred.reps + matrix(runif(length(pred.reps),0,1e-8),ncol=nReps)
      obs = obs + runif(nObs,0,1e-8)
    }
    obs.pval = calc.obs.pval(obs=obs,pred.reps=pred.reps)
  }
  pval_sort = sort(obs.pval, index.return = T)
  y = pval_sort$x
  n = length(y)
  i = seq(1:n)
  PP = i/(n + 1)
  plotpoints = qqplot(PP, y, plot.it = F)
  alpha.prime = mean(abs(plotpoints$y - plotpoints$x))
  alpha = 1 - 2 * alpha.prime
  
  return(alpha)
  
}

################

ave_CV = function(pred.reps,pred.mode){
  
  sd.pred.reps = apply(pred.reps,1,sd,na.rm=T)
  mean.pred.reps = apply(pred.reps,1,mean)
  CV = sd.pred.reps/mean.pred.reps
  average.CV = mean(CV,na.rm=T)

  return(average.CV)
  
}
  
################

nse_meanPred = function(obs=obs,pred.reps=pred.reps){

  mean.pred.reps = apply(pred.reps,1,mean)
  
  return(nse(obs=obs,pred=mean.pred.reps))

}

################

bias.mode.rel = function(obs,pred.mode){
  
  keep = !is.na(obs)
  obs = obs[keep]
  pred.mode = pred.mode[keep]
  
  pred.mode.expected = mean(pred.mode)
  obs.expected = mean(obs)
  bias = (obs.expected - pred.mode.expected)/obs.expected
  return(bias)
}

################

bias.mean.rel = function(obs,pred.reps){

  keep = !is.na(obs)
  obs = obs[keep]
  pred.reps = pred.reps[keep,]
  
  pred.expected.ts = apply(pred.reps,1,mean)
  pred.expected = mean(pred.expected.ts)
  obs.expected = mean(obs)
  bias = (obs.expected - pred.expected)/obs.expected
  return(bias)
}

################

bias.median.rel = function(obs,pred.reps){

  keep = !is.na(obs)
  obs = obs[keep]
  pred.reps = pred.reps[keep,]
  
  pred.median.ts = apply(pred.reps,1,median)
  pred.median.expected = mean(pred.median.ts)
  obs.expected = mean(obs)
  bias = (obs.expected - pred.median.expected)/obs.expected
  return(bias)
}

################

CRPS.skillscore.meanObs = function(obs,pred.reps){

  crpsOutput = crpsFunc_naPredReps(obs=obs,pred=pred.reps)$crps_decompos
  Val = crpsOutput[1]

  crpsOutput = crpsFunc_naPredReps(obs=obs,pred=rep(mean(obs,na.rm=T),length(obs),))$crps_decompos
  baseVal = crpsOutput[1] 

  SS = (Val-baseVal) /  (-baseVal)

  return(SS)

}

################

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

  SA_NSE = 1. - sum((obs-pred)^2,na.rm=T) / sum((obs-meanObsMon[month_on_day])^2,na.rm=T)

#browser()

  return(SA_NSE)

}

################

calc_all_metrics = function(obs,pred.reps,pred.mode,perturb=F,calcCRPS=T,dates,pred.reps.base=NULL){

#browser()

  PQQ_alpha_metric = PQQ_alpha(obs=obs,pred.reps=pred.reps,perturb=perturb)

#browser()

  PQQ_alpha_metric_2 = 1. - PQQ_alpha_metric
  NSE_meanPred_metric = nse_meanPred(obs=obs,pred.reps=pred.reps)
  if (!is.null(pred.mode)){
    NSE_metric = nse(obs=obs,pred=pred.mode)
    ave_CV_metric = ave_CV(pred.mode=pred.mode,pred.reps=pred.reps)
    bias.mode.rel_metric = bias.mode.rel(obs=obs,pred.mode=pred.mode)
    SA_NSE_metric = sa_nse(obs=obs,pred=pred.mode,dates=dates,seasons='months')
#    corr_metric = cor(x=obs,y=pred.mode,use='complete.obs')
#    corr_sp_metric = cor(x=obs,y=pred.mode,use='complete.obs',method='spearman')
  } else {
    NSE_metric = NA
    ave_CV_metric = NA
    bias.mode.rel_metric = NA
    SA_NSE_metric = NA
    corr_metric = NA
    corr_sp_metric = NA
  }
  bias.mean.rel_metric = bias.mean.rel(obs=obs,pred.reps=pred.reps)
  bias.mean.rel.abs = abs(bias.mean.rel_metric)
  bias.mode.rel.abs = abs(bias.mode.rel_metric)
  bias.median.rel_metric = bias.median.rel(obs=obs,pred.reps=pred.reps)
  bias.median.rel.abs = abs(bias.median.rel_metric)
  bias.mean.mode = bias.mean.rel_metric - bias.mode.rel_metric
  if (calcCRPS){
#browser()
    crpsOutput = crpsFunc_naPredReps(obs=obs,pred=pred.reps)$crps_decompos
    crps_metric = crpsOutput[1]
    crps.reliability_metric = crpsOutput[2]
#    CRPS.skillscore.meanObs_metric = CRPS.skillscore.meanObs(obs=obs,pred.reps=pred.reps) # not working 
    CRPS.skillscore.meanObs_metric = NA

  } else {
    crps_metric = NA
    crps.reliability_metric = NA    
    CRPS.skillscore.meanObs_metric = NA
  }
  timeMean_repSD = mean(apply(pred.reps,1,sd,na.rm=T))
  timeMean_repSD_over_meanObs = timeMean_repSD/mean(obs,na.rm=T)
#  topPercentilePredReps = quantile(pred.reps,probs=0.99,names=F,na.rm=T)
#  topPercentileObs = quantile(obs,probs=0.99,names=F,na.rm=T)
#  ratioTopPercentilePredRepsObs = topPercentilePredReps/topPercentileObs
  maxPredReps = max(pred.reps,na.rm=T)
  maxObs = max(obs,na.rm=T)
  ratioMaxPredRepsObs = maxPredReps/maxObs
  meanObs = mean(obs,na.rm=T)
#  timeMean_qrange_p5_p95_meanObs = mean(apply(pred.reps,1,quantile,probs=0.95,return=F)
#                                        -apply(pred.reps,1,quantile,probs=0.05,return=F))/mean(obs,na.rm=T)
#  timeMean_qrange_p10_p90_meanObs = mean(apply(pred.reps,1,quantile,probs=0.9,return=F)
#                                    -apply(pred.reps,1,quantile,probs=0.1,return=F))/mean(obs,na.rm=T)
#  timeMean_qrange_p25_p75_meanObs = mean(apply(pred.reps,1,quantile,probs=0.75,return=F)
#                                     -apply(pred.reps,1,quantile,probs=0.25,return=F))/mean(obs,na.rm=T)

#  propZerosObs = length(which(obs==0.))/length(obs)
#  propZerosPredReps = length(which(pred.reps==0.))/length(pred.reps)
#  relError_propZeros = (propZerosPredReps-propZerosObs)/propZerosObs

  keep = !is.na(obs)
  obsKeep = obs[keep]
  pred.repsKeep = pred.reps[keep,]
  propZerosObs = length(which(obsKeep==0.))/length(obsKeep)
  propZerosPredReps = length(which(pred.repsKeep==0.))/length(pred.repsKeep)
  relError_propZeros = (propZerosPredReps-propZerosObs)/propZerosObs

  error_propZeros = propZerosPredReps-propZerosObs

  if (!is.null(pred.reps.base)){
    q_5_95_pred = apply(pred.reps,1,quantile,c(0.05,0.95),na.rm=T)
    q_5_95_base = apply(pred.reps.base,1,quantile,c(0.05,0.95),na.rm=T)
    if (length(q_5_95_pred[2,]-q_5_95_pred[1,])!=length(q_5_95_base[2,]-q_5_95_base[1,])){
      print(length(q_5_95_pred[2,]-q_5_95_pred[1,]))
      print(length(q_5_95_base[2,]-q_5_95_base[1,]))
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

  return(list(PQQ_alpha=PQQ_alpha_metric,
              PQQ_alpha2=PQQ_alpha_metric_2,
              ave_CV = ave_CV_metric,
              NSE = NSE_metric,
              NSE_meanPred = NSE_meanPred_metric, 
              SA_NSE = SA_NSE_metric,
#              corr = corr_metric,
#              corr_sp = corr_sp_metric,
              bias.mean.rel = bias.mean.rel_metric,
              bias.mean.rel.abs = bias.mean.rel.abs,
              bias.mode.rel = bias.mode.rel_metric,
              bias.mode.rel.abs = bias.mode.rel.abs,
              bias.median.rel = bias.median.rel_metric,
              bias.median.rel.abs = bias.median.rel.abs,
              bias.mean.mode = bias.mean.mode,
              crps = crps_metric,
              crps.reliability = crps.reliability_metric,
              CRPS.skillscore.meanObs = CRPS.skillscore.meanObs_metric,
              timeMean_repSD = timeMean_repSD,
              timeMean_repSD_over_meanObs = timeMean_repSD_over_meanObs,
#              topPercentilePredReps = topPercentilePredReps,
#              ratioTopPercentilePredRepsObs = ratioTopPercentilePredRepsObs,
              propZerosObs=propZerosObs,
              propZerosPredReps=propZerosPredReps,
              error_propZeros=error_propZeros,
              relError_propZeros=relError_propZeros,
              maxPredReps = maxPredReps,
              meanObs = meanObs,
              ratioMaxPredRepsObs = ratioMaxPredRepsObs,
              IQR90 = IQR90,
              IQR90_mod = IQR90_mod))#,
#              timeMean_qrange_p5_p95_meanObs=timeMean_qrange_p5_p95_meanObs,
#              timeMean_qrange_p10_p90_meanObs=timeMean_qrange_p10_p90_meanObs,
#              timeMean_qrange_p25_p75_meanObs=timeMean_qrange_p25_p75_meanObs))
}

################

add_metrics_to_bad = function(bad,lowThres=NULL,highThres=NULL,thresIndex='pred.expected',perturb=F,calcCRPS=F,baseErrorModel=NULL){
#print(thresIndex)
  for (scen in names(bad)){
    for (var.grp in names(bad[[scen]])){
      print(scen)
      bad1 = bad[[scen]][[var.grp]]
      if(!is.null(baseErrorModel)){
        tmp = strsplit(scen,'_')[[1]]
        catchment = tmp[1]
        period = tmp[4]
        baseScen = paste(catchment,'_clim_',baseErrorModel,'_',period,sep='')
        bad_base = bad[[baseScen]][[var.grp]]
       }
      thres.series.vals = bad1[[thresIndex]]
      if (is.null(bad1$metrics)){bad[[scen]][[var.grp]]$metrics=list()}
      if (is.null(bad1$metrics$allData)){bad[[scen]][[var.grp]]$metrics$allData=list()}

      # all data
      obsAll = bad1$obs
      # remove masked values
      if (is.null(bad1$mask)){
        maskAll = bad1$obs.aux[,5]
      } else { 
        maskAll = bad1$mask
      }  
      pred.repsAll = bad1$pred.reps
      if(!is.null(baseErrorModel)){
        pred.repsAll.base = bad_base$pred.reps
      } else {
        pred.repsAll.base = NULL
      }
      pred.modeAll = bad1$pred.mode
      datesAll = bad1$dates     
      obsAll[maskAll!=0]=NA
      bad[[scen]][[var.grp]]$metrics$allData = calc_all_metrics(obs=obsAll,pred.reps=pred.repsAll,pred.mode=pred.modeAll,perturb=perturb,calcCRPS=calcCRPS,dates=datesAll,pred.reps.base=pred.repsAll.base) 
#      bad[[scen]][[var.grp]]$metrics$allData$PQQ_alpha = PQQ_alpha(obs=obs,pred.reps=pred.reps) 
#      bad[[scen]][[var.grp]]$metrics$allData$ave_CV = ave_CV(pred.mode=pred.mode,pred.reps=pred.reps) 
      # low data
#      browser()

      if (!(is.null(lowThres))){
        threshold = quantile(thres.series.vals, probs = lowThres, na.rm=T)
        print(threshold)
        keep = (thres.series.vals < threshold)

#        obs = bad1$obs[keep]
#        pred.reps = bad1$pred.reps[keep,]
#        pred.mode = bad1$pred.mode[keep]
#        dates = bad1$dates[keep]
 
        obs = obsAll[keep]
        pred.reps = pred.repsAll[keep,]
        if(!is.null(baseErrorModel)){
          pred.reps.base = pred.repsAll.base[keep,]
        } else {
          pred.reps.base = NULL
        }

        pred.mode = pred.modeAll[keep]
        dates = datesAll[keep]

        bad[[scen]][[var.grp]]$metrics$lowData = calc_all_metrics(obs=obs,pred.reps=pred.reps,pred.mode=pred.mode,perturb=perturb,calcCRPS=calcCRPS,dates=dates,pred.reps.base=pred.reps.base) 
      }

      if (!(is.null(highThres))){
#      bad[[scen]][[var.grp]]$metrics$lowData$PQQ_alpha = PQQ_alpha(obs=obs,pred.reps=pred.reps) 
#      bad[[scen]][[var.grp]]$metrics$lowData$ave_CV = ave_CV(pred.mode=pred.mode,pred.reps=pred.reps) 
      # high data
#        thres.series.vals = bad1[[thresIndex]]
        threshold = quantile(thres.series.vals, probs = highThres, na.rm=T)
        print(threshold)
        keep = (thres.series.vals > threshold)

#        obs = bad1$obs[keep]
#        pred.reps = bad1$pred.reps[keep,]
#        pred.mode = bad1$pred.mode[keep]
#        dates = bad1$dates[keep]

        obs = obsAll[keep]
        pred.reps = pred.repsAll[keep,]
        if(!is.null(baseErrorModel)){
          pred.reps.base = pred.repsAll.base[keep,]
        } else {
          pred.reps.base = NULL
        }

        pred.mode = pred.modeAll[keep]
        dates = datesAll[keep]

        bad[[scen]][[var.grp]]$metrics$highData = calc_all_metrics(obs=obs,pred.reps=pred.reps,pred.mode=pred.mode,perturb=perturb,calcCRPS=calcCRPS,dates=dates,pred.reps.base=pred.reps.base) 
#      bad[[scen]][[var.grp]]$metrics$highData$PQQ_alpha = PQQ_alpha(obs=obs,pred.reps=pred.reps) 
#      bad[[scen]][[var.grp]]$metrics$highData$ave_CV = ave_CV(pred.mode=pred.mode,pred.reps=pred.reps) 
      }
      if (!(is.null(highThres))&!(is.null(lowThres))){
      # mid data
#        thres.series.vals = bad1[[thresIndex]]
        threshold_low = quantile(thres.series.vals, probs = lowThres, na.rm=T)
        print(threshold_low)
        threshold_high = quantile(thres.series.vals, probs = highThres, na.rm=T)
        print(threshold_high)
        keep = (thres.series.vals >= threshold_low & thres.series.vals <= threshold_high) 

#        obs = bad1$obs[keep]
#        pred.reps = bad1$pred.reps[keep,]
#        pred.mode = bad1$pred.mode[keep]
#        dates = bad1$dates[keep]

        obs = obsAll[keep]
        pred.reps = pred.repsAll[keep,]
        if(!is.null(baseErrorModel)){
          pred.reps.base = pred.repsAll.base[keep,]
        } else {
          pred.reps.base = NULL
        }

        pred.mode = pred.modeAll[keep]
        dates = datesAll[keep]

        bad[[scen]][[var.grp]]$metrics$midData = calc_all_metrics(obs=obs,pred.reps=pred.reps,pred.mode=pred.mode,perturb=perturb,calcCRPS=calcCRPS,dates=dates,pred.reps.base=pred.reps.base)
      }

#browser()

    }
  }
  return(bad)
}

################

add_attributes_to_bad = function(bad){
  for (scen in names(bad)){
    for (var.grp in names(bad[[scen]])){
      bad1 = bad[[scen]][[var.grp]]
      if (is.null(bad1$attributes)){bad[[scen]][[var.grp]]$attributes=list()}
      strsplit_scen = strsplit(scen,'_')[[1]]
      n = length(strsplit_scen)

#      bad[[scen]][[var.grp]]$attributes$hydroModel = strsplit_scen[(n-2)]
#      bad[[scen]][[var.grp]]$attributes$errorModel = strsplit_scen[(n-1)]
#      bad[[scen]][[var.grp]]$attributes$period = strsplit_scen[n]
#      suffix = paste('_',strsplit_scen[(n-2)],'_',strsplit_scen[(n-1)],'_',strsplit_scen[n],sep='')
#      bad[[scen]][[var.grp]]$attributes$catchment = strsplit(scen,suffix)[[1]]

      bad[[scen]][[var.grp]]$attributes$catchment = strsplit_scen[1]
      bad[[scen]][[var.grp]]$attributes$hydroModel = strsplit_scen[2]
      bad[[scen]][[var.grp]]$attributes$period = strsplit_scen[4]
    
#browser()

      errorModel = strsplit_scen[3]
      if (n>=5){
        for (i in 5:n){
          errorModel = paste(errorModel,strsplit_scen[i],sep='_')
        }
      }

#browser()

#      if (n >=5){
#        errorModel = paste(errorModel,strsplit_scen[5],sep='_')
#      }
#      if (n >=6){
#        errorModel = paste(errorModel,strsplit_scen[6],sep='_')
#      }
      bad[[scen]][[var.grp]]$attributes$errorModel = errorModel

    }
  }
  return(bad)
}

################

bad_to_df = function(bad){
  table = NULL
  for (scen in names(bad)){
    for (var.grp in names(bad[[scen]])){
      bad1 = bad[[scen]][[var.grp]]
      attributes = unlist(bad1$attributes)
      for (data in names(bad1$metrics)){
        metrics = unlist(bad1$metrics[[data]])
        new_line = c(scen,attributes,data,metrics)
        names(new_line) = c('scen',names(attributes),'data',names(metrics))
        table = rbind(table,new_line,deparse.level=0)
      }
    }
  }
  return(data.frame(table))
}

################

badPar_to_df = function(bad,parList){
  table = NULL
  for (scen in names(bad)){
    for (var.grp in names(bad[[scen]])){
      bad1 = bad[[scen]][[var.grp]]
      attributes = unlist(bad1$attributes)
      new_line = c(scen,attributes)
      for (parName in parList) {
        pars = unlist(bad1$params[[parName]])
        parVal = mean(pars)
        new_line = c(new_line,parVal)
      }
      names(new_line) = c('scen',names(attributes),parList)
      table = rbind(table,new_line,deparse.level=0)
    }
  }
  return(data.frame(table))
}

################


add_SS_to_df = function(df,baseErrorModel,baseHydroModel=NULL,calcCRPS=T){
 
  crps.SS = c()
  crps.reliability.SS = c()
  timeMean_repSD.SS = c()
  PQQ_alpha.SS = c()
  ave_CV.SS = c()
  bias.mean.rel.abs.SS = c()
  bias.mode.rel.abs.SS = c()
  bias.median.rel.abs.SS = c()
  nRows = dim(df)[1]
  for (n in 1:nRows){
    catchment = df[n,]$catchment
    hydroModel = df[n,]$hydroModel
    period = df[n,]$period
    data = df[n,]$data
    if (is.null(baseHydroModel)){baseHydroModel=hydroModel}
    base = (df$catchment==catchment) & (df$hydroModel==baseHydroModel) & (df$period==period) & (df$data==data) & (df$errorModel==baseErrorModel)
    if (length(which(base))>0){
      base_df = df[base,]
      if (calcCRPS){
      # crps SS
        var = 'crps'
        baseVal = as.double(array(base_df[[var]]))
        Val = as.double(array(df[n,][[var]]))
        crps.SS[n] = (Val-baseVal) /  (-baseVal)
      # crps rel SS
        var = 'crps.reliability'
        baseVal = as.double(array(base_df[[var]]))
        Val = as.double(array(df[n,][[var]]))
        crps.reliability.SS[n] = (Val-baseVal) /  (-baseVal)
      } else  {
        crps.SS[n] = NA
        crps.reliability.SS[n] = NA
      }
      # timeMean_repSD SS
      var = 'timeMean_repSD'
      baseVal = as.double(array(base_df[[var]]))
      Val = as.double(array(df[n,][[var]]))
      timeMean_repSD.SS[n] = (Val-baseVal) /  (-baseVal)
      # PQQ alpha
      var = 'PQQ_alpha'
      baseVal = as.double(array(base_df[[var]]))
      Val = as.double(array(df[n,][[var]]))
      PQQ_alpha.SS[n] = (Val-baseVal) /  (baseVal)
      # ave CV SS
      var = 'ave_CV'
      baseVal = as.double(array(base_df[[var]]))
      Val = as.double(array(df[n,][[var]]))
      ave_CV.SS[n] = (Val-baseVal) /  (-baseVal)
      # mean bias abs
      var = 'bias.mean.rel.abs'
      baseVal = as.double(array(base_df[[var]]))
      Val = as.double(array(df[n,][[var]]))
      bias.mean.rel.abs.SS[n] = (Val-baseVal) /  (-baseVal)
      # mode bias abs
      var = 'bias.mode.rel.abs'
      baseVal = as.double(array(base_df[[var]]))
      Val = as.double(array(df[n,][[var]]))
      bias.mode.rel.abs.SS[n] = (Val-baseVal) /  (-baseVal)
      # median bias abs
      var = 'bias.median.rel.abs'
      baseVal = as.double(array(base_df[[var]]))
      Val = as.double(array(df[n,][[var]]))
      bias.median.rel.abs.SS[n] = (Val-baseVal) /  (-baseVal)
    } else {
      crps.SS[n] = NA
      crps.reliability.SS[n] = NA
      crps.reliability.SS[n] = NA
      timeMean_repSD.SS[n] = NA
      PQQ_alpha.SS[n] = NA
      ave_CV.SS[n] = NA
      bias.mean.rel.abs.SS[n] = NA
      bias.mode.rel.abs.SS[n] = NA
      bias.median.rel.abs.SS[n] = NA
    }
  }
  df = cbind(df,crps.SS,crps.reliability.SS,timeMean_repSD.SS,PQQ_alpha.SS,ave_CV.SS,
             bias.mean.rel.abs.SS,bias.mode.rel.abs.SS,bias.median.rel.abs.SS)
  return(df)  
}
