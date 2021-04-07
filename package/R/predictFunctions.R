############################################################################

calc_BC_pred_from_eta = function(Qh,A,lambda,eta,Qmin=0.,Qmax=999.,truncType='spike'){



  if (truncType == 'spike'){
    if (lambda>=0){
      Qundef = Qmin
    } else {
      Qundef = Qmax
    }
  } else if (truncType == 'resample') {
    Qundef = NA
  }
  if (lambda==0){
    pred = exp(eta)*(Qh+A)-A
  } else {

#    Y = eta + Qh
#    pred = (Y*lambda+1)^(1/lambda)
    Y = lambda*eta+(Qh+A)^lambda
    pred = Y^(1/lambda)-A
    #    if (any(Y<0)){browser()}
    #pred[Y<0]=Qundef
   # browser()
   # pred[is.nan(pred)] = Qmin 
   # pred[pred<Qmin] = Qmin # To avoid negative flows
  }
  if (truncType == 'spike'){
    pred[pred<Qmin]=Qmin
    pred[pred>Qmax]=Qmax
  } else if (truncType == 'resample') {
    pred[pred<Qmin]=NA
    pred[pred>Qmax]=NA
  }



  #if (any(is.na(pred))){browser()}

  return(pred)
}

############################################################################

calc_WLS_pred_from_eta = function(Qh,A,eta,Qmin=0.,Qmax=999.,truncType='spike'){
  pred = Qh + eta*(Qh+A)
  if (truncType == 'spike'){
    pred[pred<Qmin]=Qmin
    pred[pred>Qmax]=Qmax
  } else if (truncType == 'resample') {
    pred[pred<Qmin]=NA
    pred[pred>Qmax]=NA
  }
  return(pred)
}

############################################################################

calc_LogSinh_pred_from_eta = function(Qh,A,B,eta,Qmin=0.,Qmax=999.,truncType='spike'){
  Y = eta + calc_LogSinh_tranz(Q=Qh,A=A,B=B)
  pred = calc_inv_LogSinh_tranz(Y=Y,A=A,B=B)
  if (truncType == 'spike'){
    pred[pred<Qmin]=Qmin
    pred[pred>Qmax]=Qmax
  } else if (truncType == 'resample') {
    pred[pred<Qmin]=NA
    pred[pred>Qmax]=NA
  }
  return(pred)
}

############################################################################

calc_pred_from_eta = function(Qh,heteroModel,param,eta,Qmin=0.,Qmax=999.,truncType='spike'){

  #  browser()

  if (heteroModel == 'BC'){
    if (is.list(param)){
      pred = calc_BC_pred_from_eta(Qh=Qh,A=param$A,lambda=param$lambda,eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    } else {
      pred = calc_BC_pred_from_eta(Qh=Qh,A=param['A'],lambda=param['lambda'],eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    }
  } else if (heteroModel == 'LogSinh'){
    if (is.list(param)){
      pred = calc_LogSinh_pred_from_eta(Qh=Qh,A=param$A,B=param$B,eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    } else {
      pred = calc_LogSinh_pred_from_eta(Qh=Qh,A=param['A'],B=param['B'],eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    }
  } else if (heteroModel == 'WLS'){
    if (is.list(param)){
      pred = calc_WLS_pred_from_eta(Qh=Qh,A=param$A,eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    } else {
      pred = calc_WLS_pred_from_eta(Qh=Qh,A=param['A'],eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    }
  }

  #  browser()

  return(pred)
}

############################################################################

sim_AR1 = function(nT,mu,sigma,rho){
  eta = vector(length = nT)
  eta[1] = mu[1] + rnorm(n=1,mean=0.,sd=sigma/sqrt(1-rho^2))
  flag = 0
  for (t in 2:nT){
    if (is.na(mu[t-1]) || is.na(mu[t])) {
      eta[t] = NA
      flag = 1
    } else {
      if (flag == 1) {
        eta[t] = mu[t] + rnorm(n=1,mean=0.,sd=sigma/sqrt(1-rho^2))
        flag = 0
        next
      }
      eta[t] = rho*(eta[t-1]-mu[t-1]) + mu[t] + rnorm(n=1,mean=0.,sd=sigma)
    }
    
  }
  #browser()
  return(eta)
}

############################################################################

calc_pred_reps = function(Qh,heteroModel,param,nReps=1e2,Qmin=0.,Qmax=999.,truncType='spike',validate=F){
  nT = length(Qh)
  if (is.null(param$mean_eta_0)){
    mean_eta_0 = 0.
  } else {
    mean_eta_0 = param$mean_eta_0
  }
  if (is.null(param$mean_eta_1)){
    mean_eta_1 = 0.
  } else {
    mean_eta_1 = param$mean_eta_1
  }
  #Qht = ((Qh^0.2)-1)/0.2
  rho_eta = param$rho
  sigma_eta = param$sigma_y
  mean_eta = param$mean_eta #mean_eta_0 + mean_eta_1*Qht
  predReps = matrix(nrow=nT,ncol=nReps)
  #etaReps = matrix(nrow=nT,ncol=nReps)
  
    for (r in 1:nReps){
      eta = sim_AR1(nT,mu=mean_eta,sigma=sigma_eta,rho=rho_eta)

    #  etaReps[,r] = eta
      predReps[,r] = calc_pred_from_eta(Qh=Qh,heteroModel=heteroModel,param=param,
                                        eta=eta,Qmin=Qmin,Qmax=Qmax,truncType=truncType)
    }

    if (nReps==1){predReps=predReps[,1]}
  colnames(predReps)=paste("rep",seq(1:nReps),sep="")
    return(predReps)
}

############################################################################
