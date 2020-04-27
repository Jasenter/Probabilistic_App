## Install 'gridExtra' package first else this won't work

##################### TEST DATA #################################
#dirname = 'F:/project_DEW/webApp/' # Jason code
dirname = "C:/Users/a1654087/Box/Task1_1_PrototypeSoftware/prototype/"
data_dirname = paste(dirname,'data/',sep='')
RData_fname = paste(data_dirname,'dataB1_GR4J_SLS.csv',sep='')
#RData_fname = paste(data_dirname,'valFlow.RData',sep='')
#RData_fname = paste(data_dirname,'data.BethanyCurrent.RData',sep='')
data = read.csv(RData_fname)
#load(RData_fname)
 obs = data$obs
 sim = data$pred
#################################################################

# Source the function **step to be replaced by a package
 code_dirname = paste(dirname,'code/',sep='')
 source(paste(code_dirname,'main_function.R',sep=''))


## Arguments are the observed and the simulated data series as vectors
probabilisticMod(obs=obs,sim=sim,reps=100,dirname=dirname)