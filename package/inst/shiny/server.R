

shinyServer(function(input, output, session) {
#
  # save variables for use across multiple functions
  vals=reactiveValues(
    sim=NULL # for storing the simulation
  )
#
#   observe({
#     x=input$dataSel  # update when checkbox is click
#     print(paste("datasel is ",x))
#     print(paste("file is ",input$file1))
#     if(x=='Load my own data') {
#       inFileObs <- input$file1
#
#       print(paste("infileobs is ",inFileObs))
#
#       if (!is.null(inFileObs)){
#         data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))
#       }
#
#       maxR=ceiling(max(data$obs,na.rm=TRUE))
#       updateSliderInput(session,"yRange",label= "Y range",value = c(0,50),step=5,min=0,max=maxR)
#       maxL=length(data$obs)
#       updateSliderInput(session,"datRange",label= "X range",value = c(0,365),step=5,min=0,max=maxL)
#     }
#
#   })

  observeEvent({#input$dataset
               input$file1
               input$lab.date
               input$lab.obs
               input$lab.pred
               input$lab.unit
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
    #dataset=input$dataset     #dataset should be used to select which dataset - for now just B9 is available
    #dir.loc = system.file("shiny",package="ProbPred")
                 dir.loc = system.file("shiny",package="ProbPred")
                 demoData = read.csv(paste(dir.loc,"/","402204_SLS.csv",sep=""))
                 demoData = demoData[,2:4]
                output$file1 <- downloadHandler(filename=function(){paste("example.csv",sep="")},content=function(file){write.csv(demoData,file,row.names=F)})
    #output$dlDemo <- downloadHandler(filename=function(){paste(input$file1,"predReps",".csv",sep="")},content=function(file){write.csv(vals$out$pred.reps,file)})
    RData_fname=input$model


    #"/",metric,'.RData',sep=''
    #print(input$model)
    #if (RData_fname == "dataB1_GR4J_SLS.csv") {data = read.csv(paste(data_dirname,RData_fname,sep=""),as.is=T)}
    #if (RData_fname == "Bethany (Barossa)") {load(paste(data_dirname,"data.BethanyCurrent.RData",sep=""))}
    #if (RData_fname == "Yackandandah Creek (VIC)") {data = read.csv(paste(data_dirname,"402204_SLS.csv",sep=""),as.is=T)}
    # if (RData_fname == "data.A10_HBV_BC0.2_A0.RData") {load(paste(data_dirname,RData_fname,sep=""))}
    #if (RData_fname == "Biggara (ACT)") {data = read.csv(paste(data_dirname,"Biggara_GR4J_Log_A0.csv",sep=""),as.is=T)}
    #if (RData_fname == "Gingera (SA)") {load(paste(data_dirname,"Gingera_GR4J_Log_A0.RData",sep=""))}
    #if (RData_fname == "Lower Tanunda (Barossa)") {data = read.csv(paste(data_dirname,"Barossa_lowerTanunda_1993.csv",sep=""),as.is=T)}
    #if (RData_fname == "Lower Flaxman (Barossa)") {data = read.csv(paste(data_dirname,"Barossa_lowerFlaxman_trunc.csv",sep=""),as.is=T)}
    #if (RData_fname == "Upper Jacobs (Barossa)") {data = read.csv(paste(data_dirname,"Barossa_upperJacobs.csv",sep=""),as.is=T)}


    if (RData_fname == "Yackandandah Creek (VIC)") {data = read.csv(paste(dir.loc,"/402204_SLS.csv",sep=""),as.is=T)}

    heteroModel = "BC"  # "Box Cox"
    #if(heteroModel == "Box Cox"){heteroModel="BC"}

    meantype=input$mean

    lambda=as.numeric(input$lambda)

    #fun.dir = system.file("R",package="ProbPred")
    # print(data.lab$obs)
    # maxR=ceiling(max(data[[data.lab$obs]],na.rm=TRUE))
    # updateSliderInput(session,"yRange",label= "Y range",value = c(0,50),step=5,min=0,max=maxR)
    # maxL=length(data[[data.lab$obs]])
    # updateSliderInput(session,"datRange",label= "X range",value = c(0,365),step=5,min=0,max=maxL)

    # perfPlot=input$perfPlot
    # resPlot=input$resPlot

    # datRange=input$datRange  # has 2 values (vector)
    # yRange=input$yRange      # has 2 values (vector)

    #READ FILES
    inFileObs <- input$file1
    if (!is.null(inFileObs)){
      # return(NULL)
      data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))

      # if(all(is.na(data[input$lab.obs])) | all(is.na(data[input$lab.pred])) |
      #    all(!is.numeric(data[input$lab.obs])) | all(!is.numeric(data[input$lab.pred]))) {
      #   xerr = "Error: Data for observed and/or simulated flows not found - please specify the correct column heading in the input file that corresponds to these datasets"
      #   print(xerr)
      #   showNotification(xerr)
      #   return()
      # } else {
      #print(paste("datetime is ",input$lab.date))
      #print(paste("obs is ",input$lab.obs))
      #print(paste("pred is ",input$lab.pred))
      #print(paste("unit is ",input$lab.unit))
      data.lab = list(obs=input$lab.obs,pred=input$lab.pred,date=input$lab.date,unit=input$lab.unit)
      #print(data.lab)
      #}

      # if(input$lab.obs!="" & input$lab.pred!="" & input$lab.date!="") {
      #
      # } else {
      #   data.lab = list(obs="obs",pred="pred",date="date")
      #   #print("WARNING! no header inputs recieved - defaults set to dates = 'date', observed data = 'obs', predicted data = 'pred'")
      # }
    } else {
      data.lab = list(obs="obs",pred="pred",date="date",unit="mm/d")
      #print(data.lab)
    }

######################################

## Replacing error code data (-9999) with NA
#
#     data[[data.lab$obs]][data[[data.lab$obs]]<0] = NA # setting all negative flows to NA (asu)
#     data[[data.lab$pred]][data[[data.lab$pred]]<0] = NA
#
#     # data[[data.lab$obs]][data[[data.lab$obs]]==-9999] = NA
#     # data[[data.lab$pred]][data[[data.lab$pred]]==-9999] = NA
#
#     meanObs=mean(data[[data.lab$obs]],na.rm=TRUE)
#     A=Astar*meanObs
#     # SET ERROR MODEL PARAMETERS
#     paramFix = list(A=A,lambda=lambda)
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
      if (!is.null(inFileObs)){
        data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))
        #input.headers = c(data.lab$obs,data.lab$pred,data.lab$date)
        data.headers = colnames(data)

        if(!is.element(data.lab$obs,data.headers) |
           !is.element(data.lab$pred,data.headers) |
           !is.element(data.lab$date,data.headers)) {
          xerr(flag=1)# headers not in the data file
          return()
        } else if(all(is.na(data[data.lab$obs][[1]])) | all(is.na(data[data.lab$pred][[1]])) | all(is.na(data[data.lab$date][[1]]))) {
          xerr(flag=2) # checks for empty data vectors
          return()

        } else if(is.character(data[data.lab$obs][[1]]) | is.character(data[data.lab$pred][[1]]) | is.numeric(data[data.lab$date][[1]])) {
          xerr(flag=3) # check for characters (e.g. dates) in the obs or pred
          return()
        } else if(sum(data[data.lab$obs][[1]]-data[data.lab$pred][[1]])==0) {
          xerr(flag=4) # check for obs and pred being the same vector
          return()
        } else if(length(data[data.lab$obs][[1]])!=length(data[data.lab$pred][[1]]) |
                  length(data[data.lab$obs][[1]])!=length(data[data.lab$date][[1]])) {
          xerr(flag=5) # checks that the input vectors are all the same length
          return()
        }
      }
      # str(data)
      # print(paste("data.lab$obs is ",data.lab$obs,sep=""))
      # print(is.character(data[data.lab$obs][[1]]))
      #print(data[data.lab$obs][[1]])
      #             print(all(is.character(data[data.lab$obs])))
      #             print(all(!is.numeric(data[data.lab$obs])))
      #             print(all(!is.numeric(data[data.lab$pred])))
      #             print(which(!is.numeric(data[data.lab$obs])))
      #             print(data[data.lab$obs][[1]][1])
      #             print(data[data.lab$pred][[1]][1])
      # xerr = "Error: Data for observed and/or simulated flows not found - please specify the correct column heading in the input file that corresponds to these datasets"
      # print(xerr)
      # showNotification(xerr,type="error")
      ## Catching errors in the data
      data[[data.lab$obs]][data[[data.lab$obs]]<0 | is.infinite(data[[data.lab$obs]]) | is.nan(data[[data.lab$obs]])] = NA # setting all negative flows to NA (asu)
      data[[data.lab$pred]][data[[data.lab$pred]]<0 | is.infinite(data[[data.lab$pred]]) | is.nan(data[[data.lab$pred]])] = NA

      # data[[data.lab$obs]][data[[data.lab$obs]]==-9999] = NA
      # data[[data.lab$pred]][data[[data.lab$pred]]==-9999] = NA


      withProgress(message="Calculating...",value=0, {
        # SET ERROR MODEL PARAMETERS
        meanObs=mean(data[[data.lab$obs]],na.rm=TRUE)
        A=Astar*meanObs
        paramFix = list(A=A,lambda=lambda)

      # calibrate remaining parameters (sigma here)
      param = auxiliary(callfunction="calibrate_hetero",data=data,opt=data.lab,param=paramFix,meantype=meantype)
      #param = calibrate_hetero(data=data,param=paramFix,heteroModel=heteroModel,calc_rho=T,meantype=meantype,opt=data.lab)
      incProgress(amount=0.2)

      std.resids = auxiliary(callfunction="calc_std_resids",data=data,opt=data.lab,param=param)
      #std.resids = calc_std_resids(data=data,param=param,heteroModel=heteroModel,opt=data.lab)

      # calc pred reps
      pred.reps = auxiliary(callfunction="calc_pred_reps",data=data,opt=data.lab,param=param)
      #pred.reps = calc_pred_reps(Qh=data[[data.lab$pred]],heteroModel=heteroModel,param=param,nReps=1e2,Qmin=0.,Qmax=999.,truncType='spike')
      incProgress(amount=0.4)
      #calc pred pls
      pred.pl = auxiliary(callfunction="calc.problim",pred.reps=pred.reps)
      #pred.pl = calc.problim(pred.reps,percentiles=c(0.05,0.25,0.5,0.75,0.95))
      updateSliderInput(session,"yRange",label= "Y range",value = c(0,ceiling(max(pred.pl,na.rm=TRUE))),step=5,min=0,max=ceiling(max(pred.pl,na.rm=TRUE)))
      incProgress(amount=0.6)
      #calculate Metrics
      metrics = auxiliary(callfunction="calc_metrics",data=data,opt=data.lab,pred.reps=pred.reps)
      #metrics = calc_metrics(data=data,pred.reps=pred.reps,opt=data.lab)
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
      incProgress(amount=1.0,message='code complete!')
       })
      })

   #STORE PARS, PRED AND RESIDS

    out=list(pred.reps=pred.reps,pred.pl=pred.pl,std.resids=std.resids,param=param,metrics=metrics,data=data,meanObs=meanObs,report=report,heteroModel=heteroModel,data.lab=data.lab) #resPlot=resPlot,perfPlot=perfPlot,datRange=datRange,yRange=yRange,
    vals$out=out # save

  })
# DOWNLOADING DATA
  output$dlReps <- downloadHandler(filename=function(){paste(input$model,"predReps",".csv",sep="")},content=function(file){write.csv(vals$out$pred.reps,file)})
  output$dlPL <- downloadHandler(filename=function(){paste(input$model,"ProbLimits",".csv",sep="")},content=function(file){write.csv(vals$out$pred.pl,file)})
  output$dlSummary <- downloadHandler(filename=function(){paste(input$model,"Summary",".pdf",sep="")},content=function(file){
     pdf(file=file)
     #frontpage(inputName=input$model,param=vals$out$param,metrics=vals$out$metrics)
     #msg.print=error.print(vals$out$data)
     #error.write(data=vals$out$data,is.data=T)
     dir.loc = system.file("shiny",package="ProbPred")
     withProgress(message="preparing pdf summary...",value=0,{
       auxiliary(callfunction="output.main",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,metrics=vals$out$metrics,dir.loc=dir.loc)
       auxiliary(callfunction="boxplotter",metrics=vals$out$metrics,dir.loc=dir.loc,type.label="reliability",box.colour="pink")
       auxiliary(callfunction="boxplotter",metrics=vals$out$metrics,dir.loc=dir.loc,type.label="sharpness",box.colour="white")
       auxiliary(callfunction="boxplotter",metrics=vals$out$metrics,dir.loc=dir.loc,type.label="bias",box.colour="lightblue")
       #print("main + boxplots ok")
       #output.main(param=vals$out$param,metrics=vals$out$metrics,data=vals$out$data,is.data=T,opt=vals$out$data.lab)
       #boxplotter(data_dirname=dir.loc,catchmentMetric=vals$out$metrics[[1]],metric="reliability",boxColour="pink")
       #boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[2]],metric="sharpness",boxColour="white")
       #boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[3]],metric="bias",boxColour="lightblue")
       auxiliary(callfunction="plot.performance",data=vals$out$data,opt=vals$out$data.lab,pred.reps=vals$out$pred.reps)
       #plot.performance(data=vals$out$data,pred.reps=vals$out$pred.reps,type='PQQ',opt=vals$out$data.lab)
       incProgress(amount=0.5,message="preparing pdf plots...")
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label="pred",std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='prob(pred)',std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='density',std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='tranz',std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='extratranz',std.resids=vals$out$std.resids)

       # plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='prob(pred)',opt=vals$out$data.lab)
       # plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='density',opt=vals$out$data.lab)
       # plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='tranz',opt=vals$out$data.lab)
       # plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='extratranz',opt=vals$out$data.lab)
       #
       if (!is.na(min(vals$out$data[[vals$out$data.lab$obs]])) && !is.na(min(vals$out$data[[vals$out$data.lab$pred]]))) { # Only print these if there's no missing data
         auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='acf',std.resids=vals$out$std.resids)
         auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='pacf',std.resids=vals$out$std.resids)
         # plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='acf',opt=vals$out$data.lab)
         # plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,model=vals$out$heteroModel,param=vals$out$param,type='pacf',opt=vals$out$data.lab)
       }
       auxiliary(callfunction="timeseries",data=vals$out$data,opt=vals$out$data.lab,pred.reps=vals$out$pred.reps)
       #timeseries(data=vals$out$data,pred.reps=vals$out$pred.reps,opt=vals$out$data.lab)
       #
     incProgress(amount=1.0,message="pdf ready for download!")
       })
     dev.off()},contentType="application/pdf")

  #=========================
#RENDERING STUFF TO UI NOW
#=========================
  withProgress(message="Plotting plots...",value=0, {
   output$report <- renderTable({
     vals$out$report  #table of parameters
   },align='c',width="500",digits=3)
  output$error1 <- renderText({
    msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
    #msg.print = error.print(data=vals$out$data,opt=vals$out$data.lab)
 #   if(msg.print[1]!= "No issues found!"){tags$head(tags$style(paste("Zero flow check. ",msg.print[1],sep=""){"color: red"}))}})
    paste(msg.print[1],sep="")})
  output$error2 <-renderText({
    msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
    #msg.print = error.print(data=vals$out$data,opt=vals$out$data.lab)
    paste(msg.print[2],sep="")})
  output$error3 <-renderText({
    msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
    #msg.print = error.print(data=vals$out$data,opt=vals$out$data.lab)
    paste(msg.print[3],sep="")})
  # output$error4 <-renderText({
  #   msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
  #   #msg.print = error.print(data=vals$out$data,opt=vals$out$data.lab)
  #   paste(msg.print[4],sep="")})
  output$error5 <-renderText({
    msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
    #msg.print = error.print(data=vals$out$data,opt=vals$out$data.lab)
    paste(msg.print[5],sep="")})
  output$error6 <-renderText({
    msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
    #msg.print = error.print(data=vals$out$data,opt=vals$out$data.lab)
    paste(msg.print[6],sep="")})
  incProgress(amount=0.15)

  output$TS <- renderPlot({
    #Call function to plot
    # print(head(vals$out$pred.pl))
    # print(data$obs[1])
    input$yRange
    input$datRange
    vals$out$pred.pl
    isolate({
      auxiliary(callfunction="plot.problim",data=vals$out$data,opt=vals$out$data.lab,pred.pl=vals$out$pred.pl,input=input)
      #plot.problim(obs=vals$out$data[[vals$out$data.lab$obs]],pred=vals$out$data[[vals$out$data.lab$pred]],pred.pl=vals$out$pred.pl,xlim=input$datRange,ylim=input$yRange,
      #             add.indices=F,xlab='Time',ylab=paste('Prediction (',vals$out$data.lab$unit,")",sep=""),xtype="date",date=vals$out$data[[vals$out$data.lab$date]])
      #axis(side=1,at=seq(from=1,to=length(vals$out$data$obs),by=100),labels=vals$out$data$date[seq(from=1,to=length(vals$out$data$obs),by=100)])
      })
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
      auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label=pType,std.resids=vals$out$std.resids)
      #plot.residuals(data=vals$out$data,std.resids=vals$out$std.resids,type=pType,model=vals$out$heteroModel,param=vals$out$param,opt=vals$out$data.lab)
    })
    incProgress(amount=0.45)

    output$perf <- renderPlot({
      if(input$perfPlot == "Predictive QQ plot"){pType ='PQQ'} #link up to select perf plot styles
      auxiliary(callfunction="plot.performance",data=vals$out$data,opt=vals$out$data.lab,pred.reps=vals$out$pred.reps,type.label=pType)
      #plot.performance(data=vals$out$data,pred.reps=vals$out$pred.reps,type=pType,opt=vals$out$data.lab) #good
    })
    incProgress(amount=0.6)

    output$box <- renderPlot({
      dir.loc = system.file("shiny",package="ProbPred")
      if(input$boxPlot == "Reliability"){metric="reliability";boxColour="pink";met=1}
      if(input$boxPlot == "Sharpness"){metric="sharpness";boxColour="white";met=2}
      if(input$boxPlot == "Bias"){metric="bias"; boxColour="lightblue";met=3}
      auxiliary(callfunction="boxplotter",metrics=vals$out$metrics,dir.loc=dir.loc,type.label=metric,box.colour=boxColour)
      #boxplotter(data_dirname=data_dirname,catchmentMetric=vals$out$metrics[[met]],metric=metric,boxColour=boxColour)
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

