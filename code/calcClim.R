#$#calc_lshc_clim = function(obs,nTime=NULL,nReps=1e2){
#$#
#$#  if (is.null(nTime)){nTime=length(obs)}
#$#
#$#  censor.thres=0.005
#$#  lower.pars=list(alpha=c(0.08),beta=c(0.02),m=c(-3.0),s=c(1.5))
#$#  upper.pars=list(alpha=c(1.0),beta=c(50),m=c(50),s=c(100))
#$#  pars.ini=list(alpha=c(1e-3),beta=c(1),m=c(5),s=c(0.2))
#$#  aux=list(censor.thres=c(censor.thres),var.names=c("Clim SF"),upper.pars=upper.pars,lower.pars=lower.pars)
#$#
#$#  obs = obs[!is.na(obs)]
#$#
#$##browser()
#$#
#$#  pars.est = par.est.model.mv.norm.lsh.censor(pars.ini = pars.ini,
#$#                                             lower.pars = lower.pars,
#$#                                             upper.pars=upper.pars,
#$#                                             post.eval.model=post.eval.model.uni.norm.lsh.censor,
#$#                                             aux=aux,y=obs)
#$#
#$#  pred = sim.marg.model.mv.norm.lsh.censor(pars = pars.est,
#$#                                                lower.pars = lower.pars,
#$#                                                upper.pars =upper.pars,
#$#                                                n=nReps,
#$#                                                aux = aux)
#$#
#$#  pred.reps = rep(pred,nTime)
#$#  pred.reps.lshc = matrix(pred.reps,nrow=nTime,ncol=nReps)
#$#
#$#  return(pred.reps.lshc)
#$#
#$#}
#$#
#$##############################
#$#
#$#calc_lshc_clim_monthly = function(obs,dateObs,dateClim,nReps=1e2){
#$#
#$#  z = aggregate.using.zoo(x=zoo(obs,dateObs))
#$#  date_monthly = time(z)
#$#  obs_monthly = coredata(z)
#$#
#$#  month = as.integer(format(date_monthly,'%m'))
#$#
#$#  nMonths = length(month)
#$#
#$#  pred.reps.clim = list()
#$#  for (m in 1:12){
#$#    keep = month==m
#$#    obs_keep = obs_monthly[keep]
#$#    pred.reps.clim[[m]] = calc_lshc_clim(obs_keep,nTime=1,nReps=1e2)
#$#  }
#$#
#$#  z = aggregate.using.zoo(x=zoo(rep(0,length(dateClim)),dateClim))
#$#  dateClimMonthly = time(z)
#$#  
#$#  clim = matrix(nrow=length(dateClimMonthly),ncol=nReps)
#$#  for (i in 1:length(dateClimMonthly)){
#$#    m = as.integer(format(dateClim[i],'%m'))
#$#    clim[i,] = pred.reps.clim[[m]]
#$#  } 
#$#
#$#  climList = list(obs=obs_monthly,pred.reps=clim,pred.mode=apply(clim,1,median),dates=dateClimMonthly)
#$#
#$#  return(climList)
#$#
#$#}

#############################

LL_lsh = function(Qobs,mu_eta,sigma_eta,A_lsh,B_lsh){
  o = calc_LogSinh_tranz(Q = Qobs,A = A_lsh,B = B_lsh,returnJac = T)
#browser()
  Z = o$Z
  Jac = o$Jac
#  LL = sum(log(Jac)) + sum(dnorm(x = Z,mean = mu_eta,sd = sigma_eta,log=TRUE))
  t1 = which(Qobs!=0)
  t2 = which(Qobs==0)
  LL = sum(log(Jac[t1])) + 
    sum(dnorm(x = Z[t1],mean = mu_eta,sd = sigma_eta,log=TRUE)) + 
    sum(pnorm(q=Z[t2],mean = mu_eta,sd = sigma_eta,log=TRUE))
  return(LL)
}

###########

par_to_LL_lsh = function(par,Qobs){
  mu_eta = exp(par[1])
  sigma_eta = exp(par[2])
  A_lsh = exp(par[3])
  B_lsh = exp(par[4])
  LL = LL_lsh(Qobs,mu_eta,sigma_eta,A_lsh,B_lsh)
  return(-LL)
}

#############################

calc_lshc_clim = function(obs,nTime=NULL,nReps=1e2,parInit=NULL){

  if (is.null(nTime)){nTime=length(obs)}

  if (is.null(parInit)){parInit=c(50,25,50,5)}
  parInit = log(parInit)

  obs = obs[!is.na(obs)]

  o = optim(par = parInit,fn = par_to_LL_lsh,Qobs=obs)  
  parOpt = o$par
  parOpt = exp(parOpt)
  eta = rnorm(n = nReps, mean = parOpt[1], sd = parOpt[2])
  pred = calc_inv_LogSinh_tranz(Y = eta, A = parOpt[3], B = parOpt[4])
  pred[pred<0] = 0.

  pred.reps = rep(pred,nTime)
  pred.reps.lshc = matrix(pred.reps,nrow=nTime,ncol=nReps)

  return(pred.reps.lshc)

}

#############################

calc_lshc_clim_monthly = function(obsDailyCal,dateDailyCal,obsDailyClim,dateDailyClim,nReps=1e2){

  z = aggregate.using.zoo(x=zoo(obsDailyCal,dateDailyCal))
  dateMonthlyCal = time(z)
  obsMonthlyCal = coredata(z)

  monthCal = as.integer(format(dateMonthlyCal,'%m'))

  nMonthCal = length(monthCal)

  pred.reps.clim = list()
  for (m in 1:12){
    keep = monthCal==m
    obs_keep = obsMonthlyCal[keep]
    pred.reps.clim[[m]] = calc_lshc_clim(obs_keep,nTime=1,nReps=1e2)
  }

  z = aggregate.using.zoo(x=zoo(rep(0,length(dateDailyClim)),dateDailyClim))
  dateMonthlyClim = time(z)

  clim = matrix(nrow=length(dateMonthlyClim),ncol=nReps)
  for (i in 1:length(dateMonthlyClim)){
    m = as.integer(format(dateMonthlyClim[i],'%m'))
    clim[i,] = pred.reps.clim[[m]]
  }

  z = aggregate.using.zoo(x=zoo(obsDailyClim,dateDailyClim))
  dateMonthlyClim = time(z)
  obsMonthlyClim = coredata(z)

  climList = list(obs=obsMonthlyClim,pred.reps=clim,pred.mode=apply(clim,1,median),dates=dateMonthlyClim)

  return(climList)

}


#############################


calc_marg_clim = function(obs,nTime){

  obs = obs[!is.na(obs)]

  pred.reps = rep(obs,nTime)
  pred.reps.marg = matrix(pred.reps,nrow=nTime,ncol=length(obs),byrow=T)

  return(pred.reps.marg)

}

#############################

calc_ClimDaily_dayOfYear = function(obs,dateObs,dateClim){
#  obs = obs[!is.na(obs)]
#  dateObs = dateObs[!is.na(obs)]
  doy = as.integer(format(dateObs,'%j'))
  nYr = round(length(dateObs)/365.25)
  climDOY = matrix(nrow=366,ncol=nYr)
  for (d in 1:365){
    climDOY[d,] = obs[doy==d]
  }
  climDOY[366,] = climDOY[365,]
  climDaily = matrix(nrow=length(dateClim),ncol=nYr)  
  for (t in 1:length(dateClim)){
    climDaily[t,] = climDOY[doy[t],]
  }

#ENSURE NAs REMOVED

  return(climDaily)
}

#############################

calc_Clim_ByDate = function(obs,dateObs,dateClim,aggPeriod,nReps=100){
  if (aggPeriod=='days'){
    doy = as.integer(format(dateObs,'%j'))
#    days = unique(doy)
#    nDays = length(days)
    climDOY = matrix(nrow=366,ncol=nReps)
    for (d in 1:365){
      obsSelect = obs[doy==d]
      obsSelect = obsSelect[!is.na(obsSelect)]
      if (length(obsSelect)>0){
        climDOY[d,] = sample(obsSelect,nReps,replace=T)
      } else {
        climDOY[d,] = NA
      }
    }
    climDOY[366,] = climDOY[365,]
    clim = matrix(nrow=length(dateClim),ncol=nReps)
    for (t in 1:length(dateClim)){
      clim[t,] = climDOY[doy[t],]
    }
  } else if (aggPeriod=='months'){
    moy = as.integer(format(dateObs,'%m'))
    months = unique(moy)
    nMonths = length(months)
    climMOY = matrix(nrow=12,ncol=nReps)
    for (m in 1:12){
      obsSelect = obs[moy==m]
      obsSelect = obsSelect[!is.na(obsSelect)]
      if (length(obsSelect)>0){
        climMOY[m,] = sample(obsSelect,nReps,replace=T)
      } else {
        climMOY[m,] = NA
      }
    }
    clim = matrix(nrow=length(dateClim),ncol=nReps)
    for (t in 1:length(dateClim)){
      clim[t,] = climMOY[moy[t],]
    } 
  } else { 
    clim = NULL
  }
  return(clim)
}

#############################

calc_ClimDaily_dayOfYearWindow = function(obs,dateObs,dateClim,inc){

  dObs = as.integer(format(dateObs,'%j'))
  leapYear = as.integer(is.leapyear(as.integer(format(dateObs,'%Y'))))
  nYear = round(length(dateObs)/365.25)

  day_clim = matrix(nrow=366,ncol=(2*inc+1)*nYear)
  for (d in 1:365){
    in_window = (abs(dObs-d)<=inc) |
      (dObs-d)>=(365+leapYear-inc) |
      (dObs-d+365+leapYear)<=inc
    day_clim[d,] = obs[in_window]
  }
  day_clim[366,] = day_clim[365,]
  
  nDaysClim = length(dateClim)
  clim = matrix(nrow=nDaysClim,ncol=(2*inc+1)*nYear)
  for (n in 1:nDaysClim){
    d = as.integer(format(dateClim[n],'%j') )
    clim[n,] = day_clim[d,]
  } 

  return(clim)
}

#############################

is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
#############################

#$#calc_ClimMonthly_monthOfYear = function(obs,dateObs,dateClim){
#$#  moy = as.integer(format(dateObs,'%m'))
#$#  nYr = round(length(dateObs)/12)
#$#  climMOY = matrix(nrow=12,ncol=nYr)
#$#  for (d in 1:12){
#$#    climMOY[m,] = obs[moy==m]
#$#  }
#$#  climMonthly = matrix(nrow=length(dateClim),ncol=nYr)
#$#  for (t in 1:length(dateClim)){
#$#    climMonthly[t,] = climMOY[moy[t],]
#$#  }
#$#
#$#  return(climMonthly)
#$#}
#$#
#$##############################
#$#
#$#calc_ClimMonthly_monthOfYearWindow = function(obs,dateObs,dateClim,inc){
#$#
#$#  mObs = as.integer(format(dateObs,'%m'))
#$#  nYear = round(length(dateObs)/365.25)
#$#
#$#  day_clim = matrix(nrow=366,ncol=(2*inc+1)*nYear)
#$#  for (d in 1:365){
#$#    in_window = (abs(dObs-d)<=inc) |
#$#      (dObs-d)>=(365+leapYear-inc) |
#$#      (dObs-d+365+leapYear)<=inc
#$#    day_clim[d,] = obs[in_window]
#$#  }
#$#  day_clim[366,] = day_clim[365,]
#$#
#$#  nDaysClim = length(dateClim)
#$#  clim = matrix(nrow=nDaysClim,ncol=(2*inc+1)*nYear)
#$#  for (n in 1:nDaysClim){
#$#    d = as.integer(format(dateClim[n],'%j') )
#$#    clim[n,] = day_clim[d,]
#$#  }
#$#
#$#  return(clim)
#$#}

#############################

calc_ClimDaily_dayOfYearWindow_seamless = function(QobsCal,datesCal,datesClim=NULL,inc=14){
  
  # Test for .csv or RData date formats
  if (is.na(as.Date(datesCal[1],format="%Y-%m-%d"))){datesCal = as.Date(datesCal,format = "%d/%m/%Y")} else {datesCal = as.Date(datesCal,format = "%Y-%m-%d")}
  #datesCal = as.Date(datesCal,format = "%d/%m/%Y")
  #datesCal = as.Date(datesCal,format = "%Y-%m-%d")

  if(is.null(datesClim)) {datesClim = datesCal}
  
  yearsCal = format(datesCal,'%Y')
  # browser()
  # yearsCal = datesCal
  yearsCal = as.integer(unique(yearsCal))
  n_yearsCal = length(yearsCal)
  deltas = seq(-inc,inc)
  n_deltas = length(deltas)
  clim_mat = matrix(nrow=366,ncol=(n_deltas*n_yearsCal))

  for (j in 1:365){
    counter = 1
    for (year in yearsCal){
      dateThisYear =as.Date(paste(j,'-',year,sep=''),format='%j-%Y')  
      for (delta in deltas){
        dateSel = dateThisYear + delta
        iDateSel = which(datesCal==dateSel)
        if (length(iDateSel)==1){
          clim_mat[j,counter] = QobsCal[iDateSel]
        } else {
          clim_mat[j,counter] = NA
        }
        counter = counter+1
      }
    }
  }
  clim_mat[366,] = clim_mat[365,]  

  nDaysClim = length(datesClim)
  clim = matrix(nrow=nDaysClim,ncol=(n_deltas*n_yearsCal))
  for (n in 1:nDaysClim){
    d = as.integer(format(datesClim[n],'%j') )
    clim[n,] = clim_mat[d,]
  }
  
  return(clim)
  
}

######################
