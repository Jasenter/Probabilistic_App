

############################################################################

calibrate_hetero = function(data,param,heteroModel,method='MoM',calc_rho=F,meantype,opt){

  Qobs=data[[opt$obs]]
  Qh=data[[opt$pred]]
  Qh_T = vector(length=length(Qh))

  eta = calc_eta(Qobs=Qobs,Qh=Qh,param=param,heteroModel=heteroModel) # obs - simulated
  Qh_T = calc_tranz(Q=Qh,heteroModel=heteroModel,param=param) # The transformed simulated streamflow

  if (method=='MoM'){
    p = AR1_MoM(eta=eta,Qh=Qh_T,calc_rho=calc_rho,meantype)
    param$mean_eta_0 = p$mu0
    param$mean_eta_1 = p$mu1
    param$rho = p$rho
    param$sigma_y = p$sigma
    param$mean_eta = p$mu

  } else {
    print("Invalid method selected - use MoM only")
    browser()
  }
  return(param)
}


############################################################################

AR1_MoM = function(eta,Qh=NULL,calc_rho=F,meantype){

    Nt = length(eta)
  if (meantype=="linear"){
    m = lm(eta~Qh)
    mu0 = m$coefficients[1]
    mu1 = m$coefficients[2]

  } else if (meantype=="constant"){
    mu0 = mean(eta,na.rm=T)
    mu1 = 0.
  } else if (meantype=="zero"){
    mu0 = 0.
    mu1 = 0.
  } else{
    mu0 = 0.
    mu1 = 0.
    print("WARNING: unrecognised mean parameter type provided - zero mean used.")
  }

  n = length(eta)
  mu = mu0+mu1*Qh
  eta.star = eta-mu
  s = sqrt((sum((eta.star)^2,na.rm=T))/n)

  if (calc_rho){

    ErrorlagForward <- eta.star[2:n]
    ErrorlagBackward <- eta.star[1:n-1]
    sb = sqrt((sum((ErrorlagBackward)^2,na.rm=T))/n)
    sf = sqrt((sum((ErrorlagForward)^2,na.rm=T))/n)

    rho = (sum((ErrorlagForward)*(ErrorlagBackward),na.rm=T))/((n-1)*sb*sf)

  } else {
    rho = 0.
  }
  sigma = sqrt((s^2)*(1-(rho^2))) # sigma
  return(list(mu0=mu0,mu1=mu1,rho=rho,sigma=sigma,mu=mu))
}


############################################################################
