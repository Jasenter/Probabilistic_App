# GLOBALS
library(moments)
library(shiny)
library(shinythemes)
library(grid)
library(gridExtra)

# SOURCE SCRIPTS
source('calc_metrics.R')
source('calibrateFunctions.R')
source('plot.predictiveQQ.R')
#source('plot.prob.r')
#source('plot.problim.prob.r')
source('plot.problim.R')
source('plotFunctions.R')
source('predictFunctions.R')
source('plot.residuals.multi.R')
source('residualFunctions.R')
#source("plot.residual.vs.index.R")
#source("plot.residual.qqnorm.R")
#source("plot.residual.acf.pacf.R")
source('tranzFunctions.R')
source('np.sdy.cond.x.R')
source('boxplot.ext_DM.R')
source('calcClim.R')
source('output.R')
source('error_message.R')

#dirname = 'C:/Users/a1039419/Box/1_Projects/2019_DEW_SourceModelling_RPackage/3_Tasks/Task1_1_PrototypeSoftware/prototype/' # Mark Thyer PC
#dirname = 'C:/Users/a1065639/Box Sync/2019_DEW_SourceModelling_RPackage/3_Tasks/Task1_1_PrototypeSoftware/prototype/' # David McInerney PC
dirname = 'C:/Users/Console/Box/Task1_1_PrototypeSoftware/prototype_20200225/' # Jason home pc
#dirname = "C:/Users/a1654087/Box/Task1_1_PrototypeSoftware/prototype_20200225/" # Jason work pc

data_dirname = paste(dirname,'data/',sep='')
#RData_fname = "data.BethanyCurrent.RData"
#data = read.csv(paste(data_dirname,RData_fname,sep=""),as.is=T)
#demodata = paste("Use demo data (",RData_fname,")",sep="")
#Rdata_fname = "402204 _simFlowVect_exampleData"

JScode <-
  
  "$(function() {

    setTimeout(function(){
    
    var vals = [0];
    
    var powStart = 7;
    
    var powStop = 0;
    
    for (i = powStart; i >= powStop; i--) {
    
    var val = Math.pow(10, -i);
    
    val = parseFloat(val.toFixed(8));
    
    vals.push(val);
    
    }
    
    $('#offset').data('ionRangeSlider').update({'values':vals})

}, 5)})"
