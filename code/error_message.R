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
    
  }  else if (msgFlag==6) {
    msg.print.error = paste("WARNING: Streamflow units are not recognised.",
                            "  mmd, m3s or MLd are recommended.",
                            "  Units assumed to be millimetres per day (mmd).",
                            sep="\n")
}
  return(msg.print.error)
  
}
##################################################################
# Suite of tests on the data's suitability for predicting

error.print = function(data,opt) {
  msg.print = vector(length=6)
  msg.print = rep("No issues found!",6)
  maxL=sum(data[[opt$obs]] >= 0, na.rm=TRUE) #number of all counted flows
  zeroL = sum(data[[opt$obs]] <=0,na.rm=TRUE) # number of zero flows
  naL = sum(is.na(data[[opt$obs]]))
  allL = length(data[[opt$obs]])
  bool.na = !is.na(data[[opt$obs]])
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
  
  # test for recognisable streamflow units
  x=opt$unit
  xCount=0
  print(paste("default unit is ",x,sep=" "))
  viable.units = c("mmd","mm/d","m3s","m3/s","MLd","ML/d")
  for(i in 1:length(viable.units)) {
    if(x==viable.units[i]) {xCount=xCount+1}
  }
  if(xCount<0.1) {
    msgFlag = 6
    msg.print[msgFlag] = error(msgFlag=msgFlag)
  }
  
  return(msg.print)
}

####################