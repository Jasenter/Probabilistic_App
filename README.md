NOTE: Jasenter/Probablistic_App is no longer being supported. Please contact Mark Thyer (mark.thyer@adelaide.edu.au) for information on any future releases. This repository is archived.



ProbPred is a post-processed residual error model for generating probabilistic predictions from a users' observed and simulated hydrological streamflow data. It is presented as an R-function and as an interactive Shiny interface. 

To use the model, please download the latest release package and install it into your R library.

The following code sample may be used to run the interface and the R-function:

#### Interactive shiny interface

```
?probPredInteractive
probPredInteractive()
```
All arguments to the interface are added through the interface itself.

For best results, please view the interface in the browser window (use the 'Open in Browser' option at the top of the screen of the interface window once the above lines have been run).

#### R function

```
?probPred
probPred(data,opt,param)
```

+ 'data' is a matrix of input observed, simulated and dates of daily streamflow.
+ 'opt' is a list of input options that affect headers and titles, output format, and units.
+ 'param' are the fixed input parameters from literature.

For more information regarding the inputs and outputs of the model, please refer to the help files of the individual functions.

#### Versions
ProbPred release version 1.0.0 is optimised for:

+ R version 4.0.2
+ moments version 0.14
+ shiny version 1.6.0
+ shinythemes version 1.2.0
