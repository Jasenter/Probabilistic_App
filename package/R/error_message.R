#' Error message repository
#'
#' For internal use with the shiny app only. By J Hunter & Team.
#' @param flag  # indicates which error is being called
#' @keywords none
#' @export
#' @examples
#' none          ## no example - this script is not for the user; it is for the webapp - and webapps need no examples


################################
## Data check error messages

xerr = function(flag) {

  if(flag==1) {

    xerror = "Fatal error: Data not found under the headers provided - please check the headers are correct (case sensitive) and try again."
    print(xerror)
    showNotification(xerror)

  } else if(flag==2) {

    xerror = "Fatal error: One or more of the input vectors (e.g. observed or simulated flow or dates) contains no data."
    print(xerror)
    showNotification(xerror)

  } else if(flag==3) {

    xerror = "Fatal error: One or more of the input vectors (e.g. observed or simulated flow or dates) contains data of the wrong type - dates must be characters and flows must be numeric."
    print(xerror)
    showNotification(xerror)

  } else if(flag==4) {

    xerror = "Fatal error: Observed and simulated data are the same vector."
    print(xerror)
    showNotification(xerror)

  } else if(flag==5) {

    xerror = "Fatal error: Observed and simulated flows and / or the corresponding dates are different lengths."
  }

}
#################################

#################################
error = function(msgFlag,num=NA) {


# Too many zeroes in the time series (>50%)
    if (msgFlag==1) {
      msg.print.error = paste("WARNING:", round(num), "% of the observed streamflow are zero flows.",
                              "Predictions may be of low quality.",
                                 "Please see Mcinerney et al. 2019 for details.",
                                 sep=" ")

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
                            "  mm/d, m3/s or ML/d are recommended.",
                            "  Units assumed to be millimetres per day (mmd).",
                            sep="\n")
}
  return(msg.print.error)

}
##################################################################
# Suite of tests on the data's suitability for predicting

error.print = function(data,opt) {
  msg.print = vector(length=2)
  msg.out = rep("No issues found!",2)
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
  msgFlag=1
  if ((fracZero) > 0.05) {
    msg.out[msgFlag] = error(msgFlag=msgFlag,num=(fracZero*100))
  }
  msg.print[msgFlag] = paste("Zero flow check. ",msg.out[msgFlag],sep="")

# Test for length of time series
  msgFlag = 2
  if (maxL <= 300) {
    msg.out[msgFlag] = error(msgFlag=msgFlag)
  }
  msg.print[msgFlag] = paste("Missing data check. ",msg.out[msgFlag],sep="")

# Test for influential points
  # msgFlag = 3
  # msg.print[msgFlag] = paste("Significant data points check. ",msg.out[msgFlag],sep="")

# Test for large gaps in data
  # msgFlag = 4
  # if (max.gap >= (0.05*allL)) {
  #   msg.out[msgFlag] = error(msgFlag=msgFlag)
  # }
  # msg.print[msgFlag] = paste("Gaps in data check. ",msg.out[msgFlag],sep="")

# Count for number of missing data
  # msgFlag = 5
  # if (naL > 0) {
  #   msg.out[msgFlag] = error(msgFlag=msgFlag,num=naL)
  # }
  # msg.print[msgFlag] = paste("Missing data check. ",msg.out[msgFlag],sep="")

# test for recognisable streamflow units
  # x=opt$unit
  # xCount=0
  # viable.units = c("mmd","mm/d","m3s","m3/s","MLd","ML/d")
  # for(i in 1:length(viable.units)) {
  #   if(x==viable.units[i]) {xCount=xCount+1}
  # }
  # msgFlag = 6
  # if(xCount<0.1) {
  #   msg.out[msgFlag] = error(msgFlag=msgFlag)
  # }
  # msg.print[msgFlag] = paste("Streamflow units check. ",msg.out[msgFlag],sep="")

  return(msg.print)
}

####################


