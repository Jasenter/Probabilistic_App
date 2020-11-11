################################################

#homeloc = "C:/Users/Terminal/Desktop/R/prototype" # laptop
#homeloc = "F:/project_DEW/prototype" # desktop
dirname = "C:/Users/Console/Box/Task1_1_PrototypeSoftware/Probabilistic_App/" # desktop
################################################

library(devtools)  # builds the package
library(roxygen2)  # simplifies writing the documentation

#Creates the package.  an R-project is generated into which all the scripts should be linked  
##DONT RUN THESE TWO LINES MORE THAN ONCE ELSE IT WILL OVERWRITE THE EXISTING PACKAGE ##
# setwd(dirname)
# create("package")

# builds the package
#Sys.setenv(BINPREF = "G:/Programs/R/RTools/Rtools/mingw_$(WIN)/bin/")
Sys.setenv(BINPREF = "G:/Programs/R2020/RTools/rtools40/mingw_$(WIN)/bin/")
Sys.setenv(PATH = paste("G:/Programs/R2020/RTools/rtools40/mingw_$(WIN)/bin/","G:/Programs/R2020/RTools/rtools40/",Sys.getenv("PATH"),sep=";"))
build(paste(dirname,"package",sep=""),vignettes=F)

#Updates documentation changes only
pkgloc = paste(dirname,"package",sep="")
setwd(pkgloc)
document()
## WARNING: ensure that there is an 'export()' tag in every script with roxygen documentation


#install the package for testing 
setwd(dirname)
install("package")
install.packages("C:/Users/Console/Box/Task1_1_PrototypeSoftware/Probabilistic_App/ProbPred_1.0.tar.gz", repos = NULL, type = "source", lib="G:/Programs/R2020/R-4.0.2/library")

library(package)


# Testing the prototype package
data_dirname = paste(dirname,'data/',sep='')
RData_fname = "402204_SLS.csv" # Yackandandah Creek
data = read.csv(paste(data_dirname,RData_fname,sep=""),as.is=T)
opt = list(reps=100,
           dirname = dirname,
           title = "replicate",
           obs = "obs", 
           pred = "pred",
           date = "date", 
           lambda = 0.2,
           A = 0.,
           meantype = "linear",
           inputName = RData_fname,
           unit = "m3/s",
           repPrint=T,
           plPrint=T,
           useExampleData=T
)

probabilisticMod(data=data,
                 opt=opt)
?probabilisticMod