error = function(msgFlag,num=NA) {
  

# Too many zeroes in the time series (>50%) 
    if (msgFlag==1) {
      msg.print.error = paste("WARNING:", round(num), "% of the observed streamflow are zero flows.",  
                              "Predictions may be of low quality.",
                                 "Please see Mcinerney et al. 2019 for details.",
                                 sep="\n")

# Very short time series, or lots of missing data (<300 data points total)          
  } else if (msgFlag==2) {
    msg.print.error = paste("WARNING: Insufficient number of datapoints (<300) to obtain high-quality predictions.",
                            "Predictions may be of low quality.",
                             "Please see xyz reference for details.",
                               sep="\n")

# Potential significant points        
  } else if (msgFlag==3) {
    msg.print.error = paste("WARNING: Significant datapoints in the observed streamflow detected!",
                               "Please expect the probabilistic and hydrologic predictions to be strongly influenced by these points.",
                               sep="\n")

# Large gaps of missing data (>5% of total observed series)
  } else if (msgFlag==4) {
    msg.print.error = paste("WARNING: Large gaps in observed data (>5% of total observed data).",
                            "Predictions may be of poor quality, and some plots may not print correctly.",
                            sep="\n")
  
# Some missing data
  } else if (msgFlag==5) {
    msg.print.error = paste("WARNING: ", num, " data points missing from the observed time series.",
                            "Predictions may be of poor quality, and some plots may not print correctly.",
                            sep="\n")
}  
  return(msg.print.error)
  
}
##################################################################
# Suite of tests on the data's suitability for predicting

error.print = function(data) {
  msg.print = vector(length=5)
  msg.print = rep("No issues found!",5)
  maxL=sum(data$obs >= 0, na.rm=TRUE) #number of all counted flows
  zeroL = sum(data$obs <=0,na.rm=TRUE) # number of zero flows
  naL = sum(is.na(data$obs))
  allL = length(data$obs)
  bool.na = !is.na(data$obs)
  if(!is.na(min(bool.na))) {max.gap = 0
  } else {  max.gap = max(with(rle(bool.na), lengths[-c(1,length(lengths))][!values[-c(1,length(values))]]))
  }
  # Test for too many zero flows
  fracZero = zeroL/maxL
  if ((fracZero) > 0.05) {
    msgFlag=1
    msg.print[msgFlag] = error(msgFlag=msgFlag,num=(fracZero*100))
  }
  # Test for too many missing datapoints
  if (maxL <= 300) {
    msgFlag = 2
    msg.print[msgFlag] = error(msgFlag=msgFlag)
  }
  # Test for influential points
  
  # Test for large gaps in data
  if (max.gap >= (0.05*allL)) {
    msgFlag = 4
    msg.print[msgFlag] = error(msgFlag=msgFlag)
  }
  
  # Count for number of missing data
  if (naL > 0) {
    msgFlag = 5
    msg.print[msgFlag] = error(msgFlag=msgFlag,num=naL)
  }
  
  return(msg.print)
}

####################