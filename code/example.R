## Install 'gridExtra' package first else this won't work
#time1 = Sys.time()
##################### TEST DATA #################################
#dirname = 'C:/Users/a1654087/Box/Task1_1_PrototypeSoftware/prototype/'
dirname = 'C:/Users/Console/Box/Task1_1_PrototypeSoftware/Probabilistic_App/'

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

  opt = list(reps=100,
             dirname = dirname,
             title = "replicate",
             obs = "obs", #3,# column number (from left to right) of the observed data
             pred = "pred", #4,# data number (from left to right)  of the predicted data
             date = "date", #2,# data number (from left to right)  of the dates
             lambda = 0.2,
             A = 0.,
             meantype = "linear",
             inputName = RData_fname
  )
  # names(opt) = c("reps","dirname","title")
  # opt$reps = 100
  # opt$dirname = dirname
  # opt$title = "replicate"
  # opt$obs = "obs" 
  # opt$pred = "pred" 
  # opt$date = "Date" 
  # 
  # opt$lambda = 0.2
  # opt$A = 0
  # opt$meantype = "linear"
  # opt$inputName = RData_fname

  
## Arguments are the observed and the simulated data series as vectors
  probabilisticMod(data=data, # data frame of inputs.  Note: input headings MUST be 'date', 'obs', 'pred' for the code to run (case-sensitive).
                   opt=opt)
