#' auxiliary function
#'
#' For internal use with the shiny app only. By J Hunter & Team.
#' @param data  # input data
#' @param opt # list of options
#' @param callfunction # specifies which function to be called
#' @param param # parameters
#' @param pred.reps # replicates
#' @param heteroModel # unused
#' @param calc_rho # unused
#' @param meantype # zero, fitted or linear
#' @param metrics # metrics
#' @param dir.loc # location of data
#' @param type.label # labelling
#' @param std.resids # standardised residuals
#' @param pred.pl # probability limits
#' @param input # axis ranges
#' @keywords none
#' @export
#' @examples
#' none          ## no example - this script is not for the user; it is for the webapp




#######################################
## Interfacing for the shiny app

auxiliary = function(callfunction,data=NULL,opt=NULL,param=NULL,pred.reps=NULL,heteroModel="BC",calc_rho=T,meantype=NULL,metrics=NULL,
                     dir.loc=NULL,type.label=NULL,box.colour=NULL,std.resids=NULL,pred.pl=NULL,input=NULL) {

## Modelling functions
  if(callfunction=="calibrate_hetero") {

    param = calibrate_hetero(data=data,param=param,heteroModel=heteroModel,calc_rho=T,meantype=meantype,opt=opt)
    return(param)

  } else if (callfunction=="calc_std_resids") {

    std.resids = calc_std_resids(data=data,param=param,heteroModel=heteroModel,opt=opt)
    return(std.resids)

  } else if (callfunction=="calc_pred_reps") {

    pred.reps = calc_pred_reps(Qh=data[[opt$pred]],heteroModel=heteroModel,param=param,nReps=1e2,Qmin=0.,Qmax=999.,truncType='spike')
    return(pred.reps)

  } else if (callfunction=="calc.problim") {

    pred.pl = calc.problim(pred.reps,percentiles=c(0.05,0.25,0.5,0.75,0.95))
    return(pred.pl)

  } else if (callfunction=="calc_metrics") {

    metrics = calc_metrics(data=data,pred.reps=pred.reps,opt=opt)
    return(metrics)


## Output / plotting functions
  } else if (callfunction=="output.main") {

    output.main(param=param,metrics=metrics,data=data,is.data=T,opt=opt,dir.loc=dir.loc)


  } else if (callfunction=="boxplotter") {

    boxplotter(data_dirname=dir.loc,catchmentMetric=metrics,metric=type.label,boxColour=box.colour)

  } else if (callfunction=="plot.performance") {

    plot.performance(data=data,pred.reps=pred.reps,type='PQQ',opt=opt)

  } else if (callfunction=="plot.residuals") {

    plot.residuals(data=data,std.resids=std.resids,model=heteroModel,param=param,type=type.label,opt=opt)

  } else if (callfunction=="timeseries") {

    timeseries(data=data,pred.reps=pred.reps,opt=opt)

  } else if (callfunction=="error.print") {

    error.print(data=data,opt=opt)

  } else if (callfunction=="plot.problim") {

    plot.problim(obs=data[[opt$obs]],pred=data[[opt$pred]],pred.pl=pred.pl,xlim=input$datRange,ylim=input$yRange,
                 add.indices=F,xlab='Time',ylab=paste('Prediction (',opt$unit,")",sep=""),xtype="date",date=data[[opt$date]])



  }










}
