## Install 'gridExtra' package first else this won't work
#time1 = Sys.time()
##################### TEST DATA #################################
dirname = 'C:/Users/a1654087/Box/Task1_1_PrototypeSoftware/prototype/'
#dirname = 'C:/Users/Console/Box/Task1_1_PrototypeSoftware/prototype/'

data_dirname = paste(dirname,'data/',sep='')

########### Data input ##############
#RData_fname = "Barossa_lowerFlaxman.csv" # Lower Flaxman (Barossa)
#RData_fname = "Barossa_lowerTanunda_1993.csv" # Part of Lower Tanunda (Barossa)
#RData_fname = "Barossa_upperJacobs.csv" # Upper Jacobs (Barossa)
#RData_fname = "Biggara_GR4J_Log_A0.csv" # Biggara (ACT) 
RData_fname = "402204_SLS.csv" # Yackandandah Creek
data = read.csv(paste(data_dirname,RData_fname,sep=""),as.is=T)
#################################################################
 
 
# Source the function **step to be replaced by a package
 code_dirname = paste(dirname,'code/',sep='')
 source(paste(code_dirname,'main_function.R',sep=''))

  opt = list(1,1,1,1,1,1,1)
  names(opt) = c("reps","dirname","title")
  opt$reps = 100
  opt$dirname = dirname
  opt$title = "replicate"
  #opt$header = 5 # 5 lines of header in the rev.csv
  opt$lambda = 0.2
  opt$A = 0
  opt$meantype = "linear"
  opt$inputName = RData_fname
  
  source(paste(code_dirname,'main_function.R',sep=''))
## Arguments are the observed and the simulated data series as vectors
  probabilisticMod(data=data, # data frame of inputs.  Note: input headings MUST be 'date', 'obs', 'pred' for the code to run (case-sensitive).
                   opt=opt)
