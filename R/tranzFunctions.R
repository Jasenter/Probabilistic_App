############################################################################

calc_BC_tranz = function(Q,A,lambda,returnJac=F){
  if (lambda==0){
    Z = log(Q+A)
    Jac = 1/(Q+A)
  } else {
    Z = ((Q+A)^lambda-1)/lambda
    Jac = (Q+A)^(lambda-1)
  }
  if(returnJac){
    return(list(Z=Z,Jac=Jac))
  } else {
    return(Z)
  }
}

############################################################################

calc_WLS_tranz = function(Q,Qh,A,returnJac=F){
  Z = Q/(Qh+A)
  Jac = 1/(Qh+A)
  if(returnJac){
    return(list(Z=Z,Jac=Jac))
  } else {
    return(Z)
  }
}

############################################################################

calc_LogSinh_tranz = function(Q,A,B,returnJac=F){

  Z = B*log(sinh((Q+A)/B))
  if(returnJac){
    return(list(Z=Z,Jac=Jac))
  } else {
    return(Z)
  }
}

############################################################################

calc_inv_LogSinh_tranz = function(Y,A,B){
  Z_inv = B*asinh(exp(Y/B))-A
  return(Z_inv)
}

############################################################################

calc_tranz = function(Q,Qh=NULL,heteroModel='BC',param,returnJac=F){

  if (heteroModel == 'BC'){
    if (is.list(param)){
      Z = calc_BC_tranz(Q=Q,A=param$A,lambda=param$lambda)

    } else {
      Z = calc_BC_tranz(Q=Q,A=param['A'],lambda=param['lambda'],returnJac=returnJac)
    }
  } else if (heteroModel == 'WLS'){
    if (is.list(param)){
      Z = calc_WLS_tranz(Q=Q,Qh=Qh,A=param$A)
    } else {
      Z = calc_WLS_tranz(Q=Q,Qh=Qh,A=param['A'],returnJac=returnJac)
    }
  }
  return(Z)
}

############################################################################
