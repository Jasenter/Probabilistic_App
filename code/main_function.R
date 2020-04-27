probabilisticMod = function(data,opt) {
  
  
  #rm(list = ls(all = TRUE))
  require("gridExtra")
  require("moments")
  require("grid")
  #######################################
  ## Inputs
 
  setwd(dirname)

  
  reps = opt$reps
  title = opt$title
  dirname = opt$dirname
  
  
  #######################################
  code_dirname = paste(dirname,'code/',sep='')
  data_dirname = paste(dirname,'data/',sep='')
  source(paste(code_dirname,'plot.predictiveQQ.R',sep=''))
  source(paste(code_dirname,'plot.residuals.multi.R',sep=''))
  source(paste(code_dirname,'plot.problim.R',sep=''))
  source(paste(code_dirname,'residualFunctions.R',sep=''))
  source(paste(code_dirname,'predictFunctions.R',sep=''))
  source(paste(code_dirname,'calibrateFunctions.R',sep=''))
  source(paste(code_dirname,'tranzFunctions.R',sep=''))
  source(paste(code_dirname,'plotFunctions.R',sep=''))
  source(paste(code_dirname,'calc_metrics.R',sep=''))
  source(paste(code_dirname,'boxplot.ext_DM.R',sep=''))
  source(paste(code_dirname,'np.sdy.cond.x.R',sep=''))
  source(paste(code_dirname,'error_message.R',sep=''))
  source(paste(code_dirname,'calcClim.R',sep=''))
  source(paste(code_dirname,'output.R',sep=''))
  source(paste(code_dirname,'error_message.R',sep=''))
  
  #data = list(obs,sim)
  #names(data) = c("obs","pred")
  #browser()
  # set error model params
  heteroModel = 'BC'
  #A = 0
  #lambda = 0.2
  
  paramFix = list(A=opt$A,lambda=opt$lambda)
  meantype = opt$meantype
  data$obs[data$obs==-9999] = NA
  data$pred[data$pred==-9999] = NA
  #calc_mu0 = T # calculate intercept of mu
  #calc_mu1 = T # calculate slope of mu
  calc_rho = T # Always calculate Rho
  
  
  ######################################
  ## Error checks on the observed data
  # msg.print = vector(length=4)
  # msg.print = rep(NA,4)
  # maxL=sum(data$obs >= 0, na.rm=TRUE) #number of all counted flows
  # zeroL = sum(data$obs <=0,na.rm=TRUE) # number of zero flows
  # allL = length(data$obs)
  # bool.na = !is.na(data$obs)
  # if(!is.na(min(bool.na))) {max.gap = 0
  # } else {  max.gap = max(with(rle(bool.na), lengths[-c(1,length(lengths))][!values[-c(1,length(values))]]))
  # }
  # 
  # 
  # # Test for too many zero flows
  # if ((zeroL/maxL) > 0.5) {
  #   msgFlag=1
  #   msg.print[msgFlag] = error(msgFlag=msgFlag)
  # }
  # # Test for too many missing datapoints
  # if (maxL <= 300) {
  #   msgFlag = 2
  #   msg.print[msgFlag] = error(msgFlag=msgFlag)
  # }
  # # Test for influential points
  # 
  # # Test for large gaps in data
  # if (max.gap >= (0.05*allL)) {
  #   msgFlag = 4
  #   msg.print[msgFlag] = error(msgFlag=msgFlag)
  # }

  ######################################
  
  
  # calc parameters
  param = calibrate_hetero(data=data,param=paramFix,heteroModel=heteroModel,calc_rho=T,meantype=meantype)
  
  # calc eta_star
  std.resids = calc_std_resids(data=data,param=param,heteroModel=heteroModel)

  # calc pred reps
  pred.reps = calc_pred_reps(Qh=data$pred,heteroModel=heteroModel,param=param,nReps=reps,Qmin=0.,Qmax=999.,truncType='spike')

  # print replicates
  
  pred.pl = calc.problim(pred.reps,percentiles=c(0.05,0.25,0.5,0.75,0.95))
  #write.csv(pred.pl,file=paste(title,"_PL.csv",sep=""))
  #write.csv(x=pred.reps,file=paste(title,"_PredReps.csv",sep=""))

 # generating metrics (reliability, precision, bias)
   metrics = calc_metrics(data=data,pred.reps=pred.reps)


#pdf.options(title="Probabilistic predictions")
   pdf(paste(title,"_Summary.pdf",sep=""))
# 
   # Printing first page
   frontpage(inputName=opt$inputName,param=param,metrics=metrics)
   # Printing second page (errors)
   
   #msg.print=error.print(data)
   error.write(data=data,is.data=T)

  # Boxplots
   boxplotter(data_dirname=data_dirname,catchmentMetric=metrics$reliability,metric="reliability",boxColour="pink")
   boxplotter(data_dirname=data_dirname,catchmentMetric=metrics$sharpness,metric="sharpness",boxColour="white")
   boxplotter(data_dirname=data_dirname,catchmentMetric=metrics$bias,metric="bias",boxColour="lightblue")
   # 
   plot.performance(data=data,pred.reps=pred.reps,type='PQQ')
   # 
   plot.residuals(data=data,std.resids=std.resids,type='pred')
   #
   plot.residuals(data=data,std.resids=std.resids,type='prob(pred)')
   # 
   plot.residuals(data=data,std.resids=std.resids,type='density')
   #
   tranzplotter(data=data,param=param,heteroModel=heteroModel,add.legend=T,add.title=T)
   #

   if (!is.na(min(data$obs)) && !is.na(min(data$pred))) {
   #if (msg.print[5] == "No issues found! ") { # Only print these if there's no missing data
     acfplotter(data=data,acfType='acf',param=param,heteroModel=heteroModel)
     acfplotter(data=data,acfType='pacf',param=param,heteroModel=heteroModel)
   }
   # Timeseries
   timeseries(data=data,pred.reps=pred.reps)
   #

  
   
   dev.off()
  
}