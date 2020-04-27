
shinyServer(function(input, output, session) {
#  
  # save variables for use across multiple functions
  vals=reactiveValues(
    sim=NULL # for storing the simulation
  )

  observe({
    x=input$dataSel  # update when checkbox is clicked

    if(x=='Upload my own data') {
      inFileObs <- input$file1
      print(inFileObs$datapath)
      if (!is.null(inFileObs)){
        data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))
      }
      maxR=ceiling(max(data$obs,na.rm=TRUE))
      updateSliderInput(session,"yRange",label= "Y range",value = c(0,50),step=5,min=0,max=maxR)
      maxL=length(data$obs)
      updateSliderInput(session,"datRange",label= "X range",value = c(0,365),step=5,min=0,max=maxL)
    }

  })
  
  
  observeEvent({input$dataset
               input$file1
               input$lambda
               input$offset
               input$model
               input$mean
               }
               ,{

    ##########################
    ###UPDATE INFO FROM FORM##
    ##########################
    if(input$offset>0){Astar=10^-(8-input$offset)}else{Astar=0}
    dataset=input$dataset     #dataset should be used to select which dataset - for now just B9 is available
   
    RData_fname=input$model   
    print(input$model)
    #if (RData_fname == "dataB1_GR4J_SLS.csv") {data = read.csv(paste(data_dirname,RData_fname,sep=""),as.is=T)}
    if (RData_fname == "Bethany (Barossa)") {load(paste(data_dirname,"data.BethanyCurrent.RData",sep=""))}
    if (RData_fname == "Yackandandah Creek (VIC)") {data = read.csv(paste(data_dirname,"402204_SLS.csv",sep=""),as.is=T)}
    # if (RData_fname == "data.A10_HBV_BC0.2_A0.RData") {load(paste(data_dirname,RData_fname,sep=""))}
    if (RData_fname == "Biggara (SA)") {data = read.csv(paste(data_dirname,"Biggara_GR4J_Log_A0.csv",sep=""),as.is=T)}
    #if (RData_fname == "Gingera (SA)") {load(paste(data_dirname,"Gingera_GR4J_Log_A0.RData",sep=""))}
    if (RData_fname == "Lower Tanunda (Barossa)") {data = read.csv(paste(data_dirname,"Barossa_lowerTanunda_1993.csv",sep=""),as.is=T)}
    #if (RData_fname == "Lower Flaxman (Barossa)") {data = read.csv(paste(data_dirname,"Barossa_lowerFlaxman_trunc.csv",sep=""),as.is=T)}
    #if (RData_fname == "Upper Jacobs (Barossa)") {data = read.csv(paste(data_dirname,"Barossa_upperJacobs.csv",sep=""),as.is=T)}
    
    heteroModel = "BC"  # "Box Cox"
    #if(heteroModel == "Box Cox"){heteroModel="BC"}
    
    meantype=input$mean
     
    meanObs=mean(data$obs,na.rm=TRUE) 
    
    lambda=as.numeric(input$lambda)
    
    A=Astar*meanObs
    
    # perfPlot=input$perfPlot
    # resPlot=input$resPlot
    
    # datRange=input$datRange  # has 2 values (vector)
    # yRange=input$yRange      # has 2 values (vector)
    
    # SET ERROR MODEL PARAMETERS
    paramFix = list(A=A,lambda=lambda)
    
    #READ FILES
    inFileObs <- input$file1
    if (!is.null(inFileObs)){
      # return(NULL)
      data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))
    }

######################################
    
## Replacing error code data (-9999) with NA
    
    data$obs[data$obs==-9999] = NA
    data$pred[data$pred==-9999] = NA
    
## Error checks on the observed data
    # msg.print = vector(length=3)
    # msg.print = rep("NA",3)
    # maxL=sum(data$obs >= 0, na.rm=TRUE) #number of all counted flows
    # zeroL = sum(data$obs <=0,na.rm=TRUE) # number of zero flows
    # 
    # # Test for too many zero flows
    # if ((zeroL/maxL) > 0.5) {
    #   msgFlag=1
    #   msg.print[msgFlag] = error(data=data,msgFlag=msgFlag)
    # }
    # # Test for too many missing datapoints
    # if (maxL <= 300) {
    #   msgFlag = 2
    #   msg.print[msgFlag] = error(data=data,msgFlag=msgFlag)
    # }
    # Test for influential points
    
    ################
    ##CALCULATIONS##
    ################

    isolate({
      withProgress(message="Calculating residuals...",value=0, {
      # calibrate remaining parameters (sigma here)
      param = calibrate_hetero(data=data,param=paramFix,heteroModel=heteroModel,calc_rho=T,meantype=meantype)
      incProgress(amount=0.2)
      
      std.resids = calc_std_resids(data=data,param=param,heteroModel=heteroModel)
      
      # calc pred reps
      pred.reps = calc_pred_reps(Qh=data$pred,heteroModel=heteroModel,param=param,nReps=1e2,Qmin=0.,Qmax=999.,truncType='spike')
      incProgress(amount=0.4)
      #calc pred pls
      pred.pl = calc.problim(pred.reps,percentiles=c(0.05,0.25,0.5,0.75,0.95))
      updateSliderInput(session,"yRange",label= "Y range",value = c(0,ceiling(max(pred.pl,na.rm=TRUE))),step=5,min=0,max=ceiling(max(pred.pl,na.rm=TRUE)))
      incProgress(amount=0.6)
      #calculate Metrics
      metrics = calc_metrics(data=data,pred.reps=pred.reps)
      metrics=as.data.frame(metrics)
      incProgress(amount=0.8)
      #REPORTING INFO
      phi=param$rho
      sigma=param$sigma_y
      #report=data.frame(A,phi,sigma,meanObs,param$mean_eta_0,param$mean_eta_1)
      report=data.frame(A,phi,sigma,param$mean_eta_0,param$mean_eta_1)
      #greek = c(alpha='\u03b1',beta='\u03b2',mu='\u03bc',sigma='\u03c3',phi='\u03c6')
      # names(report)=c("Offset (A)",
      #                 paste("Lag-1 autoregressive coeff. (",greek['phi'],")",sep=""), # estimated correlation phi
      #                 paste("Innovation standard deviation (",greek['sigma'],")",sep=""), # Sigma_y; the estimated sigma of the innovations
      #                 #paste("Mean (",greek['mu'],")",sep=""), # the mean of the observed streamflow data
      #                 paste("Mean intercept (",greek['alpha'],")",sep=""), # estimated intercept
      #                 paste("Mean slope (",greek['beta'],")",sep="")) # estimated slope
      names(report)=c("Offset (A)",
                      paste("Lag-1 autoregressive coeff. (phi)",sep=""), # estimated correlation phi
                      paste("Innovation standard deviation (sigma)",sep=""), # Sigma_y; the estimated sigma of the innovations
                      #paste("Mean (",greek['mu'],")",sep=""), # the mean of the observed streamflow data
                      paste("Mean intercept (alpha)",sep=""), # estimated intercept
                      paste("Mean slope (beta)",sep="")) # estimated slope
      incProgress(amount=1.0)
       })
      })
    
   #STORE PARS, PRED AND RESIDS 
    out=list(pred.reps=pred.reps,pred.pl=pred.pl,std.resids=std.resids,param=param,metrics=metrics,data=data,meanObs=meanObs,report=report,heteroModel=heteroModel) #resPlot=resPlot,perfPlot=perfPlot,datRange=datRange,yRange=yRange,
    vals$out=out # save

  })
# DOWNLOADING DATA
  output$dlReps <- downloadHandler(filename=function(){paste(input$model,"predReps",".csv",sep="")},content=function(file){write.csv(vals$out$pred.reps,file)})
  output$dlPL <- downloadHandler(filename=function(){paste(input$model,"ProbLimits",".csv",sep="")},content=function(file){write.csv(vals$out$pred.pl,file)})
  output$dlSummary <- downloadHandler(filename=function(){paste(input$model,"Summary",".pdf",sep="")},content=function(file){
     pdf(file=file)
     frontpage(inputName=input$model,param=vals$out$param,metrics=vals$out$metrics)
     #msg.print=error.print(vals$out$data)
     error.write(data=vals$out$data,is.data=T)
     boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[1]],metric="reliability",boxColour="pink")
     boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[2]],metric="sharpness",boxColour="white")
     boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[3]],metric="bias",boxColour="lightblue")
     plot.performance(data=vals$out$data,pred.reps=vals$out$pred.reps,type='PQQ')
     plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='pred')
     plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='prob(pred)')
     plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='density')
     plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='tranz')
     plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='extratranz')
    # 
     if (!is.na(min(vals$out$data$obs)) && !is.na(min(vals$out$data$pred))) { # Only print these if there's no missing data
       plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='acf')
       plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='pacf')
     }
     timeseries(data=vals$out$data,pred.reps=vals$out$pred.reps)
    #
    dev.off()},contentType="application/pdf")

  #=========================    
#RENDERING STUFF TO UI NOW
#=========================  
  withProgress(message="Plotting plots...",value=0, {
   output$report <- renderTable({
     vals$out$report  #table of parameters
   },align='c',width="500",digits=3)

  output$error1 <- renderText({      
    msg.print = error.print(data=vals$out$data)
 #   if(msg.print[1]!= "No issues found!"){tags$head(tags$style(paste("Zero flow check. ",msg.print[1],sep=""){"color: red"}))}})
    paste("Zero flow check. ",msg.print[1],sep="")})
  output$error2 <-renderText({
    msg.print = error.print(data=vals$out$data)
    paste("Time-series length check. ",msg.print[2],sep="")})
  output$error3 <-renderText({
    msg.print = error.print(data=vals$out$data)
    paste("Significant data points check. ",msg.print[3],sep="")})
  output$error4 <-renderText({
    msg.print = error.print(data=vals$out$data)
    paste("Gaps in data check. ",msg.print[4],sep="")})
  output$error5 <-renderText({
    msg.print = error.print(data=vals$out$data)
    paste("Missing data check. ",msg.print[5],sep="")})
  incProgress(amount=0.15)
  
  output$TS <- renderPlot({
    #Call function to plot
    # print(head(vals$out$pred.pl))
    # print(data$obs[1])
    input$yRange
    input$datRange
    vals$out$pred.pl
    isolate({plot.problim(obs=vals$out$data$obs,pred=vals$out$data$pred,pred.pl=vals$out$pred.pl,xlim=input$datRange,ylim=input$yRange,add.indices=F,xlab='Time',ylab='Prediction',xaxs='i',yaxs='i')}) 
  })
  incProgress(amount=0.3)
  
    output$resid <- renderPlot({
      if(input$resPlot == "Standardised residuals v cummulative probability of predictions"){pType ='prob(pred)'} #link up to select perf plot styles
      if(input$resPlot == "Standardised residuals v predictions"){pType ='pred'} #link up to select perf plot styles
      if(input$resPlot == "Probability density of standardised residuals"){pType ='density'} #link up to select perf plot styles
      if(input$resPlot == "Transformed residuals"){pType ='tranz'} #link up to select perf plot styles
      if(input$resPlot == "Transformed residuals with moving statistics"){pType ='extratranz'} #link up to select perf plot styles   
      if(input$resPlot == "Autocorrelation plot of the residual innovations") {pType = 'acf'}
      if(input$resPlot == "Partial-autocorrelation plot of the residual innovations") {pType = 'pacf'}
      plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,type=pType,model=vals$out$heteroModel,param=vals$out$param) 
    })
    incProgress(amount=0.45)
    
    output$perf <- renderPlot({
      if(input$perfPlot == "Predictive QQ plot"){pType ='PQQ'} #link up to select perf plot styles
      plot.performance(data=vals$out$data,pred.reps=vals$out$pred.reps,type=pType) #good
    })
    incProgress(amount=0.6)
    
    output$box <- renderPlot({
      if(input$boxPlot == "Reliability"){metric="reliability";boxColour="pink";met=1}
      if(input$boxPlot == "Sharpness"){metric="sharpness";boxColour="white";met=2}
      if(input$boxPlot == "Bias"){metric="bias"; boxColour="lightblue";met=3}
      boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[met]],metric=metric,boxColour=boxColour)
    })
    incProgress(amount=0.75)
    # output$tranz <- renderPlot({
    #   metFlag=c(F,F,F,F,F)
    #   if(is.element('mean',input$tranzPlot)){metFlag[1] = T}
    #   if(is.element('standard deviation',input$tranzPlot)){metFlag[2] = T}
    #   if(is.element('skewness',input$tranzPlot)){metFlag[3] = T}
    #   if(is.element('excess kurtosis',input$tranzPlot)){metFlag[4] = T}
    #   if(is.element('linear model',input$tranzPlot)){metFlag[5] = T}
    #   tranzplotter(data=vals$out$data,param=vals$out$param,metFlag=metFlag,heteroModel=vals$out$heteroModel)
    # })
    
    incProgress(amount=0.9)
    output$metrics <- renderTable({
      vals$out$metrics  #table of metrics reliability, precision, bias
    },align='c',width="300",digits=3)
    incProgress(amount=1.0)
  })
})

