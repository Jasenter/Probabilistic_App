############################################################################

calc_eta = function(Qobs,Qh,param,heteroModel){
  eta = calc_tranz(Q=Qobs,Qh=Qh,param=param,heteroModel=heteroModel) - calc_tranz(Q=Qh,Qh=Qh,param=param,heteroModel=heteroModel)
  return(eta)
}

############################################################################

calc_std_resids = function(data,param,heteroModel,opt){
  Qobs = data[[opt$obs]]
  Qh = data[[opt$pred]]
  Qh_T = calc_tranz(Q=Qh,heteroModel=heteroModel,param=param) # The transformed simulated streamflow
  eta = calc_eta(Qobs,Qh,param,heteroModel)
  n = length(eta)-sum(is.na(eta))
  mu0 = param$mean_eta_0
  mu1 = param$mean_eta_1
  mu = mu0+(Qh_T*mu1)
   eta.star = eta-mu
   #s = sqrt((sum((eta.star)^2,na.rm=T))/n) # sigma_eta
   sigmaEta = param$sigma/(sqrt(1-param$rho^2))
   #sigma = s/sqrt(1.-param$rho^2) # sigmaY from standardising
   #sigma = sqrt((s^2)*(1-(param$rho^2))) # sigmaY from calibration stage
  # print(param$sigma)
  # print(paste("icept is ",mu0))
  # print(paste("slope is ",mu1))
  # print(paste("sdeta is ",sigmaEta))
  #print(paste("sdy is ",sigma))
  nu = (eta.star)/sigmaEta
  # print(mean(nu))

  return(nu)
}

############################################################################

calc_innovations = function(data,param,heteroModel,opt){
  Qobs = data[[opt$obs]]
  Qh = data[[opt$pred]]
  n=length(Qobs)
  y = vector(length=n)
  eta = vector(length=n)
  y[1] = eta[1]
  eta = calc_eta(Qobs,Qh,param,heteroModel)
  mu0 = param$mean_eta_0
  mu1 = param$mean_eta_1
  mu = mu0+Qh*mu1
  phi = param$rho


  for (i in 2:n){
    y[i] = (eta[i]-mu[i]) - (phi*(eta[i-1]-mu[i]))
  }

  return(y)
}
