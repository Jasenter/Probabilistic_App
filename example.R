## If useExampleData==T, then 'dirname' and RData_fname are 'default'.

opt = list(reps=100,
           dirname = default,
           title = "replicate",
           obs = "obs", 
           pred = "pred",
           date = "date", 
           lambda = 0.2,
           A = 0.,
           meantype = "linear",
           inputName = default,
           unit = "m3/s",
           repPrint=T,
           plPrint=T,
           useExampleData=T
)

probabilisticMod(data=data,
                 opt=opt)
?probabilisticMod