#' probabilisticMod function
#'
#' Generates error predictions from a given observed and simulated series. By J Hunter & Team.
#' @param data  # input data, given as a .csv file.  Must contain unbroken, aligned columns of a) dates, b) observed streamflow, and c) simulated streamflow.
#' @param opt # list of options.  All options must be given for now.
#' @keywords none
#' @export
#' @examples
#' probabilisticMod(data,opt)          ## An example of how to use the function

probabilisticMod = function(data,opt) {


  #rm(list = ls(all = TRUE))
  #require("gridExtra")
  #require("moments")
  #require("grid")
  #######################################
  ## Inputs

  reps = opt$reps
  title = opt$title
  dirname = opt$dirname

  setwd(dirname)


  #######################################
  code_dirname = paste(dirname,'code/',sep='')
  data_dirname = paste(dirname,'data/',sep='')
  # source(paste(code_dirname,'plot.predictiveQQ.R',sep=''))
  # source(paste(code_dirname,'plot.residuals.multi.R',sep=''))
  # source(paste(code_dirname,'plot.problim.R',sep=''))
  # source(paste(code_dirname,'residualFunctions.R',sep=''))
  # source(paste(code_dirname,'predictFunctions.R',sep=''))
  # source(paste(code_dirname,'calibrateFunctions.R',sep=''))
  # source(paste(code_dirname,'tranzFunctions.R',sep=''))
  # source(paste(code_dirname,'plotFunctions.R',sep=''))
  # source(paste(code_dirname,'calc_metrics.R',sep=''))
  # source(paste(code_dirname,'boxplot.ext_DM.R',sep=''))
  # source(paste(code_dirname,'np.sdy.cond.x.R',sep=''))
  # source(paste(code_dirname,'error_message.R',sep=''))
  # source(paste(code_dirname,'calcClim.R',sep=''))
  # source(paste(code_dirname,'output.R',sep=''))
  # source(paste(code_dirname,'error_message.R',sep=''))


  #data = list(obs,sim)
  #names(data) = c("obs","pred")
  #browser()
  # set error model params
  heteroModel = 'BC'
  #A = 0
  #lambda = 0.2

  paramFix = list(A=opt$A,lambda=opt$lambda)
  meantype = opt$meantype
  data[[opt$obs]][data[[opt$obs]]==-9999] = NA
  data[[opt$pred]][data[[opt$pred]]==-9999] = NA

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
  #write.csv(pred.pl,file=paste(title,"_PL.csv",sep=""))
  #write.csv(x=pred.reps,file=paste(title,"_PredReps.csv",sep=""))
   print("Starting calculation of metrics...")
 # generating metrics (reliability, precision, bias)
   metrics = calc_metrics(data=data,pred.reps=pred.reps,opt=opt)

   print("Printing to pdf...")
#pdf.options(title="Probabilistic predictions")
   pdf(paste(title,"_Summary.pdf",sep=""))
#
   # Printing first page
   #frontpage(inputName=opt$inputName,param=param,metrics=metrics)
   # Printing second page (errors)

   #msg.print=error.print(data=data,opt=opt)
   #error.write(msg.print=msg.print,data=data,opt=opt,is.data=T) #error.print(data=data,opt=opt)
#print("error goes to here")
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

   if (!is.na(min(data[[opt$obs]])) && !is.na(min(data[[opt$pred]]))) {
   #if (msg.print[5] == "No issues found! ") { # Only print these if there's no missing data
     acfplotter(data=data,acfType='acf',param=param,heteroModel=heteroModel,opt=opt)
     acfplotter(data=data,acfType='pacf',param=param,heteroModel=heteroModel,opt=opt)
   }
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


