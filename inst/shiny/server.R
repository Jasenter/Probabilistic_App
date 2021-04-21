

shinyServer(function(input, output) {
#
  # save variables for use across multiple functions
  vals=reactiveValues(
    sim=NULL # for storing the simulation
  )


  observeEvent({input$file1
                input$file2
               input$lab.date
               input$lab.obs
               input$lab.pred
               input$lab.unit
               input$lambda
               input$offset
               input$model
               input$mean
               input$dataSel
               }
               ,{

    ##########################
    ## UPDATE INFO FROM FORM

    if(input$offset>0){Astar=10^-(8-input$offset)}else{Astar=0}
    dir.loc = system.file("shiny",package="ProbPred")

    # PREPARING EXAMPLE DATA FOR PRINTING
    demoData = read.csv(paste(dir.loc,"/","402204_SLS.csv",sep=""))
    demoData = demoData[,2:4]

    output$file2 <- downloadHandler(filename=function(){paste("example.csv",sep="")},content=function(file){write.csv(demoData,file,row.names=F)})

    heteroModel = "BC"  # "Box Cox"

    meantype=input$mean

    lambda=as.numeric(input$lambda)
    data.lab = list(obs="obs",pred="pred",date="date",unit="mm/d")

    # READ FILES
    if(input$dataSel=="Load my own data" & !is.null(input$file1)) {
      inFileObs <- input$file1

      data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))

      data.lab = list(obs=input$lab.obs,pred=input$lab.pred,date=input$lab.date,unit=input$lab.unit)

    } else {
      RData_fname=input$model
      if (RData_fname == "Yackandandah Creek (NSE)") {data = read.csv(paste(dir.loc,"/402204_SLS.csv",sep=""),as.is=T)}
      if (RData_fname == "Yackandandah Creek (NSE-BC02)") {data = read.csv(paste(dir.loc,"/402204_BC02.csv",sep=""),as.is=T)}

    }
    #if (!is.null(inFileObs)){


    #} else {


    #}

    ##########################
    ## CALCULATIONS

    isolate({
      if (input$dataSel=="Load my own data" & !is.null(input$file1)) {
      #if (!is.null(inFileObs)){
        data=as.data.frame(read.csv(inFileObs$datapath, header=TRUE))

        data.headers = colnames(data)

        ## Error checks on the input data
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
        } else if(sum(data[data.lab$obs][[1]]-data[data.lab$pred][[1]],na.rm=T)==0) {
          xerr(flag=4) # check for obs and pred being the same vector
          return()
        } else if(length(data[data.lab$obs][[1]])!=length(data[data.lab$pred][[1]]) |
                  length(data[data.lab$obs][[1]])!=length(data[data.lab$date][[1]])) {
          xerr(flag=5) # checks that the input vectors are all the same length
          return()
        }
      }

      data[[data.lab$obs]][data[[data.lab$obs]]<0 | is.infinite(data[[data.lab$obs]]) | is.nan(data[[data.lab$obs]])] = NA # setting all negative flows to NA
      data[[data.lab$pred]][data[[data.lab$pred]]<0 | is.infinite(data[[data.lab$pred]]) | is.nan(data[[data.lab$pred]])] = NA


      withProgress(message="Calculating...",value=0, {

        # SET ERROR MODEL PARAMETERS
        meanObs=mean(data[[data.lab$obs]],na.rm=TRUE)
        A=Astar*meanObs
        paramFix = list(A=A,lambda=lambda)

      # calibrate  parameters
      param = auxiliary(callfunction="calibrate_hetero",data=data,opt=data.lab,param=paramFix,meantype=meantype)

      incProgress(amount=0.2)

      std.resids = auxiliary(callfunction="calc_std_resids",data=data,opt=data.lab,param=param)

      # calc pred reps
      pred.reps = auxiliary(callfunction="calc_pred_reps",data=data,opt=data.lab,param=param)

      incProgress(amount=0.4)

      #calc pred pls
      pred.pl = auxiliary(callfunction="calc.problim",pred.reps=pred.reps)

      incProgress(amount=0.6)

      #calculate Metrics
      metrics = auxiliary(callfunction="calc_metrics",data=data,opt=data.lab,pred.reps=pred.reps)

      metrics=as.data.frame(metrics)
      incProgress(amount=0.8)

      #REPORTING INFO
      phi=param$rho
      sigma=param$sigma_y

      report=data.frame(A,phi,sigma,param$mean_eta_0,param$mean_eta_1)

      names(report)=c("Offset (A)",
                      paste("Lag-1 autoregressive coeff. (phi)",sep=""), # estimated correlation phi
                      paste("Innovation standard deviation (sigma)",sep=""), # Sigma_y; the estimated sigma of the innovations
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
  #reps.dl =
  #pred.pl.dl =
  output$dlReps <- downloadHandler(filename=function(){paste(input$model,"predReps",".csv",sep="")},content=function(file){write.csv(cbind(date=vals$out$data$date,vals$out$pred.reps),file)})
  output$dlPL <- downloadHandler(filename=function(){paste(input$model,"ProbLimits",".csv",sep="")},content=function(file){write.csv(cbind(date=vals$out$data$date,vals$out$pred.pl),file)})
  output$dlSummary <- downloadHandler(filename=function(){paste(input$model,"Summary",".pdf",sep="")},content=function(file){
     pdf(file=file)

     dir.loc = system.file("shiny",package="ProbPred")
     withProgress(message="preparing pdf summary...",value=0,{
       auxiliary(callfunction="output.main",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,metrics=vals$out$metrics,dir.loc=dir.loc)
       auxiliary(callfunction="boxplotter",metrics=vals$out$metrics[[1]],dir.loc=dir.loc,type.label="reliability",box.colour="pink")
       auxiliary(callfunction="boxplotter",metrics=vals$out$metrics[[2]],dir.loc=dir.loc,type.label="sharpness",box.colour="white")
       auxiliary(callfunction="boxplotter",metrics=vals$out$metrics[[3]],dir.loc=dir.loc,type.label="bias",box.colour="lightblue")

       auxiliary(callfunction="plot.performance",data=vals$out$data,opt=vals$out$data.lab,pred.reps=vals$out$pred.reps)

       incProgress(amount=0.5,message="preparing pdf plots...")
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label="pred",std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='prob(pred)',std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='density',std.resids=vals$out$std.resids)
       auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='tranz',std.resids=vals$out$std.resids)
       #auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='extratranz',std.resids=vals$out$std.resids)

       #if (!is.na(min(vals$out$data[[vals$out$data.lab$obs]])) && !is.na(min(vals$out$data[[vals$out$data.lab$pred]]))) { # Only print these if there's no missing data
      #   auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='acf',std.resids=vals$out$std.resids)
      #   auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label='pacf',std.resids=vals$out$std.resids)
      # }
       auxiliary(callfunction="timeseries",data=vals$out$data,opt=vals$out$data.lab,pred.reps=vals$out$pred.reps)

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

    paste(msg.print[1],sep="")})
  output$error2 <-renderText({
    msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)

    paste(msg.print[2],sep="")})
  # output$error3 <-renderText({
  #   msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
  #
  #   paste(msg.print[3],sep="")})
  # output$error5 <-renderText({
  #   msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
  #
  #   paste(msg.print[4],sep="")})
  # output$error5 <-renderText({
  #   msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
  #
  #   paste(msg.print[5],sep="")})
  # output$error6 <-renderText({
  #   msg.print = auxiliary(callfunction="error.print",data=vals$out$data,opt=vals$out$data.lab)
  #
  #   paste(msg.print[6],sep="")})
  incProgress(amount=0.15)

  output$TS <- renderPlot({

    input$yRange
    input$datRange
    vals$out$pred.pl
    xmax=length(vals$out$data[vals$out$data.lab$obs][[1]])
    ymax = ceiling(max(vals$out$data[vals$out$data.lab$pred][[1]],na.rm=T))
    updateSliderInput(inputId="datRange",max=xmax)
    updateSliderInput(inputId="yRange",max=ymax,min=0,step=1.0)
    isolate({
      auxiliary(callfunction="plot.problim",data=vals$out$data,opt=vals$out$data.lab,pred.pl=vals$out$pred.pl,input=input)
      })
  })
  incProgress(amount=0.3)

    output$resid <- renderPlot({
      if(input$resPlot == "Standardised residuals v cummulative probability of predictions"){pType ='prob(pred)'} #link up to select perf plot styles
      if(input$resPlot == "Standardised residuals v predictions"){pType ='pred'} #link up to select perf plot styles
      if(input$resPlot == "Probability density of standardised residuals"){pType ='density'} #link up to select perf plot styles
      if(input$resPlot == "Standardised residuals v transformed predictions"){pType ='tranz'} #link up to select perf plot styles
      if(input$resPlot == "Transformed residuals with moving statistics"){pType ='extratranz'} #link up to select perf plot styles
      if(input$resPlot == "Autocorrelation plot of the residual innovations") {pType = 'acf'}
      if(input$resPlot == "Partial-autocorrelation plot of the residual innovations") {pType = 'pacf'}
      auxiliary(callfunction="plot.residuals",data=vals$out$data,opt=vals$out$data.lab,param=vals$out$param,type.label=pType,std.resids=vals$out$std.resids)

    })
    incProgress(amount=0.45)

    output$perf <- renderPlot({
      if(input$perfPlot == "Predictive QQ plot"){pType ='PQQ'} #link up to select perf plot styles
      auxiliary(callfunction="plot.performance",data=vals$out$data,opt=vals$out$data.lab,pred.reps=vals$out$pred.reps,type.label=pType)

    })
    incProgress(amount=0.6)

    output$box <- renderPlot({
      dir.loc = system.file("shiny",package="ProbPred")
      if(input$boxPlot == "Reliability"){metric="reliability";boxColour="pink";met=1}
      if(input$boxPlot == "Sharpness"){metric="sharpness";boxColour="white";met=2}
      if(input$boxPlot == "Bias"){metric="bias"; boxColour="lightblue";met=3}
      auxiliary(callfunction="boxplotter",metrics=vals$out$metrics[[met]],dir.loc=dir.loc,type.label=metric,box.colour=boxColour)

    })
    incProgress(amount=0.75)

    incProgress(amount=0.9)
    output$metrics <- renderTable({
      vals$out$metrics  #table of metrics reliability, precision, bias
    },align='c',width="300",digits=3)
    incProgress(amount=1.0)
  })
})

