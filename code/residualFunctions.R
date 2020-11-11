############################################################################

calc_eta = function(Qobs,Qh,param,heteroModel){
  eta = calc_tranz(Q=Qobs,Qh=Qh,param=param,heteroModel=heteroModel) - calc_tranz(Q=Qh,Qh=Qh,param=param,heteroModel=heteroModel)
  return(eta)
}

############################################################################

calc_std_resids = function(data,param,heteroModel,opt){
  Qobs = data[[opt$obs]]
  Qh = data[[opt$pred]]
  n=length(Qobs)
  eta = calc_eta(Qobs,Qh,param,heteroModel)
  mu0 = param$mean_eta_0
  mu1 = param$mean_eta_1
  mu = mu0+Qh*mu1
  s = sqrt((sum((eta-mu)^2,na.rm=T))/n)
  sigma = s/sqrt(1.-param$rho^2)   
  nu = (eta-mu)/sigma
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
  # s = sqrt((sum((eta-mu)^2,na.rm=T))/n)
  # sigma = s/sqrt(1.-param$rho^2) 
  
  for (i in 2:n){
    y[i] = (eta[i]-mu[i]) - (phi*(eta[i-1]-mu[i]))
  }

  return(y)
}