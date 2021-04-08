#' probabilisticMod function
#'
#' Generates error predictions from a given observed and simulated series. Outputs a .pdf summary file containing plots and metrics, and (optionally) two .csv files containing the probabilistic predictions themselves and the probability limits. By J Hunter & Team.
#' @param data  Input data, given as a matrix.  Must contain unbroken, aligned columns of a) dates, b) observed streamflow, and c) simulated streamflow from a deterministic model.
#' @param opt List of options.
#' @param opt$reps Number of probabilistic replicates to be generated. Recommended (and default) number is 100. Higher numbers of replicates require more computing time and lower numbers risk innacurate predictions.
#' @param opt$dirname Directory into which the output files are to be saved.
#' @param opt$title Default 'replicate'. The title (minus the extension) of the printed .pdf that is output from this function.
#' @param opt$obs Column name of the observed streamflow in the input data. Default is 'obs'. This input must match the column header of the observed streamflow.
#' @param opt$pred Column name of the simulated, deterministic streamflow in the input data. Default is 'pred'. This input must match the column header of the simulated streamflow.
#' @param opt$date Column name of the dates in the input data. Default is 'date'. This input must match the column header of the dates.
#' @param opt$meantype Options are "zero" or "linear", with "linear" being default. Defines the structure of the mean parameter in the probabilistic model. "zero" uses traditional modelling assumptions, and "linear" represents an innovative new approach that is more generally-applicable to a wider range of objective functions. Please refer to Hunter et al. 2021 for a comprehensive demonstration of the difference in predictive quality between the two.
#' @param opt$unit Units of the input streamflow. Default is 'mmd'. Alternatives are 'mm/d', 'm3s', 'm3/s','ML/d', 'MLd'.
#' @param opt$repPrint T / F to print a .csv containing the probabilistic replicates. Default is 'T'.
#' @param opt$plPrint T / F to print a .csv containing the probability limits. Default is 'T'.
#' @param param List of parameters.
#' @param param$A Box-Cox shift parameter.
#' @param param$lambda Box-Cox power parameter.
#' @keywords none
#' @export
#' @examples
#' probabilisticMod(data,opt,param)             ## An example of how to launch the function.
#'
#' data = read.csv('inputDataFile.csv',as.is=T) ## Example of setting up the data file.
#'
#' opt = list(reps=100,
#'            title='myProbPredictions',
#'            obs='observedData',
#'            pred='predictedData',
#'            date='dates',
#'            meantype='linear',
#'            unit='ML/d',
#'            repPrint=T,
#'            plPrint=T)                        ## Example of setting up the user options.
#'
#' param = list(A=0.,
#'              lambda=0.2)                     ## Example of setting up the input parameters.

probabilisticMod = function(data,opt,param) {


  x="moments" %in% rownames(installed.packages())
  if(!x) {install.packages("moments",lib=.libPaths())}
  library("moments")

  x="shiny" %in% rownames(installed.packages())
  if(!x) {install.packages("shiny",lib=.libPaths())}
  library("shiny")

  x="shinythemes" %in% rownames(installed.packages())
  if(!x) {install.packages("shinythemes",lib=.libPaths())}
  library("shinythemes")

  #######################################
  ## Inputs

  reps = opt$reps
  title = opt$title
  dirname = opt$dirname

  setwd(opt$dirname)
  data_dirname = system.file("shiny",package="ProbPred")

  #######################################

  # set error model params
  heteroModel = 'BC'

  paramFix = list(A=param$A,lambda=param$lambda)
  meantype = opt$meantype
  data[[opt$obs]][data[[opt$obs]]<0 | is.infinite(data[[opt$obs]]) | is.nan(data[[opt$obs]])] = NA
  data[[opt$pred]][data[[opt$pred]]<0 | is.infinite(data[[opt$pred]]) | is.nan(data[[opt$pred]])] = NA


  calc_rho = T # Always calculate Rho

  ######################################


  # calc parameters
  param = calibrate_hetero(data=data,param=paramFix,heteroModel=heteroModel,calc_rho=T,meantype=meantype,opt=opt)

  # calc eta_star
  std.resids = calc_std_resids(data=data,param=param,heteroModel=heteroModel,opt=opt)

  print("Starting calculation of probabilistic replicates...")
  # calc pred reps
  pred.reps = calc_pred_reps(Qh=data[[opt$pred]],heteroModel=heteroModel,param=param,nReps=reps,Qmin=0.,Qmax=999.,truncType='spike')

  # print replicates

  pred.pl = calc.problim(pred.reps,percentiles=c(0.05,0.25,0.5,0.75,0.95))

   print("Starting calculation of metrics...")
 # generating metrics (reliability, precision, bias)
   metrics = calc_metrics(data=data,pred.reps=pred.reps,opt=opt)

   print("Printing to pdf...")

   pdf(paste(title,"_Summary.pdf",sep=""))
#

   output.main(param=param,metrics=metrics,data=data,is.data=T,opt=opt,dir.loc=data_dirname) # printing new front page
  # Boxplots
   boxplotter(data_dirname=data_dirname,catchmentMetric=metrics$reliability,metric="reliability",boxColour="pink")
   boxplotter(data_dirname=data_dirname,catchmentMetric=metrics$sharpness,metric="sharpness",boxColour="white")
   boxplotter(data_dirname=data_dirname,catchmentMetric=metrics$bias,metric="bias",boxColour="lightblue")
   #
   plot.performance(data=data,pred.reps=pred.reps,type='PQQ',opt=opt)
   #
   plot.residuals(data=data,std.resids=std.resids,type='pred',opt=opt)
   #
   plot.residuals(data=data,std.resids=std.resids,type='prob(pred)',opt=opt)
   #
   plot.residuals(data=data,std.resids=std.resids,type='density',opt=opt)
   #
   tranzplotter(data=data,param=param,heteroModel=heteroModel,add.legend=T,add.title=T,opt=opt)
   #

   ## Prints auto & partial correlation plots
   #if (!is.na(min(data[[opt$obs]])) && !is.na(min(data[[opt$pred]]))) {
     #acfplotter(data=data,acfType='acf',param=param,heteroModel=heteroModel,opt=opt)
     #acfplotter(data=data,acfType='pacf',param=param,heteroModel=heteroModel,opt=opt)
   #}
   # Timeseries
   timeseries(data=data,pred.reps=pred.reps,opt=opt)
   #



   dev.off() # terminate PDF

   if(opt$repPrint==T) { # Print out a .csv with the replicates in it
     write.csv(x=pred.reps,file=paste(title,"_replicates.csv",sep=""))
   }
   if(opt$plPrint==T) { # Print out a .csv with the probability limits in it
     write.csv(x=pred.pl,file=paste(title,"_probLimits.csv",sep=""))
   }
   print("Run complete!  Please check directory for output files.")
}


