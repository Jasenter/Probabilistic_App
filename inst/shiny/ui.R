##########################
## Javascript Code object
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

##########################
## Installing packages
x="moments" %in% rownames(installed.packages())
if(!x) {install.packages("moments")}
library("moments")

x="shiny" %in% rownames(installed.packages())
if(!x) {install.packages("shiny")}
library("shiny")

x="shinythemes" %in% rownames(installed.packages())
if(!x) {install.packages("shinythemes")}
library("shinythemes")

####NOTE #####
#ON THIS SIDE OF THE APP (UI) WITHIN THE SHINYUI FUCTION LINES INSIDE FUNCTIONS (AND FUNCTIONS THEMSELVES) ARE SEPARATED USING COMMAS

##########################
## Defining the UI
shinyUI(
  
  
  ##########################
  ## Front 'About' page
  navbarPage("Interactive Probabilistic Predictions",theme = shinytheme("united"),
             ##########################
             #FIRST TAB PANEL: Home
             
             tabPanel("Home",
                      helpText(h3("Features")),
                      helpText(HTML("<ul><li> Simple and easy to use interactive webapp for single site analysis </li></ul>")),
                      helpText(HTML("<ul><li> Provides high quality probabilistic predictions that are robust to a wide range of common objective functions </li></ul>")),
                      helpText(HTML("<ul><li> Incorporates the latest research advances in residual error model selection to handle common features of predictive errors, (see 'Help/About') </li></ul>")),
                      helpText(HTML("<ul><li> Evaluates predictive performance using a range of commonly used metrics and diagnostics </li></ul>")),
                      helpText(HTML("<ul><li> Users simply upload a time series of predictions and observations (see 'Getting Started') </li></ul>")),
                      helpText(HTML("<ul><li> Users can easily download time series of probabilistic predictions, probability limits and summary metrics </li></ul>")),
                      helpText(HTML("<ul><li> Command-line functionality in R package provides opportunity for automated analysis of a large number of sites </li></ul>")),
                      helpText(h3("Benefits")),
                      helpText(HTML("<ul><li> Probabilistic predictions provide realistic estimates of water resource system risks - without uncertainty, risks are under-estimated providing a false sense of security  </li></ul>")),
                      img(src='benefitsimg.jpg',align='center',style='width: 70vw; min-width: 330px;'),
                      helpText(HTML("<ul><li> Incorporating the uncertainty enables decision-makers with different attitudes to risk aversion to act differently if aware of the uncertainty </li></ul>")),
                      helpText(HTML("<ul><li> Encourages the modeller to think about the modelling processes and the quality of information used to inform decisions  </li></ul>")),
                      helpText(HTML("<ul><li> Decision-makers and the public have the 'right to know' all limitations of a design/analysis in order to make up their own minds and lobby for their individual causes </li></ul>")),
                      helpText(HTML("Inspired by <a href='https://doi.org/10.1029/2005WR004820'> Pappenberger & Beven 2006 </a> and <a href = ' https://doi.org/10.1002/2016WR019129'> Vogel & Farmer 2016 </a>.")),
                      helpText(h3("Development Team")),
                      helpText(h4("Lead developer")),
                      helpText(HTML("<a href= 'https://researchers.adelaide.edu.au/profile/jason.hunter'> Jason Hunter </a>, PhD Candidate, School of Civil, Environmental and Mining Engineering, University of Adelaide (<a href='jason.hunter@adelaide.edu.au'>Email</a>)")),
                      helpText(h4("Contributors")),
                      helpText(HTML("<a href='https://researchers.adelaide.edu.au/profile/mark.thyer'> Dr Mark Thyer </a>, Associate Professor in Water Resources Engineering, School of Civil, Environmental and Mining Engineering, University of Adelaide (<a href='mark.thyer@adelaide.edu.au'>Email</a>)")),
                      helpText(HTML("<a href = 'https://researchers.adelaide.edu.au/profile/david.mcinerney'> Dr David McInerney </a>, Senior Research Associate, School of Civil, Environmental and Mining Engineering, University of Adelaide (<a href='david.mcinerney@adelaide.edu.au'>Email</a>)")),
                      helpText(HTML("<a href = 'https://researchers.adelaide.edu.au/profile/dmitri.kavetski'> Prof. Dmitri Kavetski </a>, Professor in Environmental Modelling, School of Civil, Environmental and Mining Engineering, University of Adelaide (<a href='dmitri.kavetski@adelaide.edu.au'>Email</a>)")),
                      helpText(HTML("<a href = 'https://researchers.adelaide.edu.au/profile/bree.bennett'> Dr Bree Bennett </a>, Senior Lecturer, School of Civil, Environmental and Mining Engineering, University of Adelaide (<a href='bree.bennett@adelaide.edu.au'>Email</a>)"))
             ),
             
             ##########################
             #SECOND TAB PANEL: Getting started
             tabPanel("Getting Started",
                      helpText(h3("1.   Prepare and upload data")),
                      helpText(HTML("<ul><li> Users need to calibrate their own hydrological model to streamflow 'observations' and use the model to generate streamflow 'predictions' for their catchment of interest </li></ul>")),
                      helpText(HTML("<ul><li> Prepare a data file (csv format) of the streamflow 'predictions' and 'observations' for upload to the webapp </li></ul>")),
                      helpText(HTML("<ul><li> See 'Simulation | Input Data' for details and an example input file  </li></ul>")),
                      helpText(h3("2.   Evaluate and enhance predictive data")),
                      helpText(HTML("<p style='color:red'> Note: To view videos, please open the interface in the browser by clicking the option 'Open in Browser' at the top of the interface window </p>")),
                      helpText("Introduction to evaluating probabilistic predictions: What makes a good probabilistic prediction?"),
                      HTML('<iframe src="https://widgets.figshare.com/articles/14430905/embed?show_title=1" width="568" height="351" allowfullscreen frameborder="0"></iframe>'),
                      helpText("Introduction to residual diagnostics to evaluate error model assumptions"),
                      HTML('<iframe src="https://widgets.figshare.com/articles/14430956/embed?show_title=1" width="568" height="351" allowfullscreen frameborder="0"></iframe>'),
                      helpText("Enhancing predictive performance"),
                      HTML('<iframe src="https://widgets.figshare.com/articles/14430965/embed?show_title=1" width="568" height="351" allowfullscreen frameborder="0"></iframe>'),
                      helpText("Practical guidance on representing uncertainty in hydrological predictions"),
                      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/mvuYlyF6S4s" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                      helpText(h3("3.   Output")),
                      helpText(HTML("<ul><li> Replicates: CSV data file of multiple individual time series (replicates) of probabilistic streamflow predictions generated by the residual error model - useful to be used as input into other models </li></ul>")),
                      helpText(HTML("<ul><li> Probability Limits: CSV data file of the probability limits (5%, 95%) of the probabilistic predictions - useful for plotting purposes </li></ul>")),
                      helpText(HTML("<ul><li> Summary: PDF file that summarises the analysis, includes input data details, summary metrics and diagnostics, and probabilistic time series </li></ul>"))
             ),
             
             
             ##########################
             ## THIRD TAB PANEL: Simulation
             tabPanel("Simulation", icon=icon("area-chart","fa-1.9x"),  #adding an icon to the tab
                      
                      verticalLayout(
                        
                        wellPanel(
                          
                          # HEADINGS
                          fluidRow(
                            column(4,helpText(h3("Input Data"))),
                            conditionalPanel(condition = "input.dataSel == 'Use demo data'",
                                             column(5,selectInput(inputId="model",label="demo data select",
                                                                  choices =c("Yackandandah Creek (NSE)",
                                                                             "Yackandandah Creek (NSE-BC02)"),
                                                                  selected="Yackandandah Creek (NSE)")))
                          ),
                          br(),
                          
                          # INPUT DATA
                          fluidRow(
                            column(5,radioButtons(inputId="dataSel",label="",choices=c("Use demo data","Load my own data"),selected="Use demo data",inline = TRUE),
                                   downloadLink('file2',label="data file example - use 'Open in Browser' at top of interface for easy viewing")) # downloads demo data from package
                          ),
                          fluidRow(
                            
                            conditionalPanel(condition = "input.dataSel == 'Load my own data'",
                                             column(3,
                                                    fileInput('file1',label="",accept=c('text/csv','text/comma-separated-values/plain','.csv')),
                                                    textInput(inputId="lab.date",label="header of dates",value="date"),
                                                    textInput(inputId="lab.obs",label="header of observed data",value="obs"),
                                                    textInput(inputId="lab.pred",label="header of predicted data",value="pred"),
                                                    textInput(inputId="lab.unit",label="input units (e.g. mm/d, m3/s, ML/d)"),value="mm/d")
                            )
                          )
                        ), # end of well panel
                        
                        
                        # MODEL PARAMETERS
                        wellPanel(
                          fluidRow(
                            column(7,helpText(h3("Residual Model Parameters")))
                          ),
                          
                          fluidRow(
                            column(3,sliderInput("lambda","Transformation power parameter (Lambda)",min=0, max=1,value=0.2,step=0.1),
                                   tags$head(tags$script(HTML(JScode))),
                                   sliderInput("offset", "Transformation offset parameter [Dimensionless] (A*)",min = 0,max = 1e-0,value = 0.0001)),
                            column(2,selectInput(inputId="mean",label="mean structure",choices=c("linear","constant","zero"),selected="zero")),
                            column(5,div(tableOutput("report"),style="font-size:120%"))
                          )
                          
                        ),
                        
                        # WARNING MESSAGES
                        
                        wellPanel(
                          fluidRow(
                            column(7,helpText(h3("Data integrity checks")))
                          ),
                          fluidRow(
                            column(7,textOutput(outputId="error1")),
                            column(7,textOutput(outputId="error2"))
                            #column(7,textOutput(outputId="error3")),
                            #column(7,textOutput(outputId="error5")),
                            #column(7,textOutput(outputId="error6"))
                          )
                        ),
                        
                        # TIME SERIES PLOT
                        wellPanel(
                          
                          fluidRow(
                            column(4,checkboxInput(inputId="plotTS",label="Plot timeseries",value=TRUE)) #tick box to hide plot & plotting options
                          ),
                          fluidRow(
                            conditionalPanel(condition = "input.plotTS == true",
                                             plotOutput("TS"),  #outputting a graphic beneath (option to turn off)
                                             column(8,sliderInput("datRange","X range",min=0, max=1000,value=c(50,450),step=1)),
                                             column(4,sliderInput("yRange","Y range",min=0, max=30,value=c(0,30),step=1.0,round=TRUE))
                            )
                          )
                        ),
                        
                        # PREDICTIVE PERFORMANCE EVALUATION PLOTS
                        wellPanel(
                          fluidRow(
                            column(4,checkboxInput(inputId="plotEval",label="Plot predictive performance evaluation",value=TRUE)) #tick box to hide plot & plotting options
                          ),
                          
                          conditionalPanel(condition = "input.plotEval==true",
                                           
                                           fluidRow(
                                             column(6,helpText("Performance metric plot type")),
                                             column(6,helpText("Performance benchmarking"))
                                           ),
                                           
                                           fluidRow(
                                             column(6,selectInput("perfPlot",NULL,choices =c("Predictive QQ plot"),selected="Predictive QQ plot")),
                                             column(6,selectInput("boxPlot",NULL,choices = c("Reliability","Sharpness","Bias"),selected="Reliability"))
                                             
                                           ),
                                           
                                           fluidRow(
                                             column(6,plotOutput("perf")),  #far-left
                                             column(6,plotOutput("box"))   #centre-right
                                           ),
                                           
                                           
                                           fluidRow(
                                             column(5,helpText("")),
                                             column(2,helpText(h4(HTML("</P> <P ALIGN=CENTER>Metric Summary</P> "))))
                                           ),
                                           
                                           fluidRow(
                                             column(5,helpText("")),
                                             column(2,div(tableOutput("metrics"),style="font-size:120%")) #right
                                           )
                          ) # end tick-box condition
                        ), #end well panel
                        
                        # RESIDUAL DIAGNOSTICS PLOTS
                        wellPanel(
                          fluidRow(
                            column(4,checkboxInput(inputId="plotResid",label="Plot residual diagnostics",value=TRUE)) #tick box to hide plot & plotting options
                          ),
                          conditionalPanel(condition = "input.plotResid==true",
                                           
                                           fluidRow(
                                             column(3,helpText("Residual plot type"))
                                           ),
                                           fluidRow(
                                             column(4,selectInput("resPlot",NULL,
                                                                  choices =c("Standardised residuals v predictions",
                                                                             "Standardised residuals v cummulative probability of predictions",
                                                                             "Probability density of standardised residuals",
                                                                             "Standardised residuals v transformed predictions"),
                                                                  #"Autocorrelation plot of the residual innovations",
                                                                  #"Partial-autocorrelation plot of the residual innovations"),
                                                                  selected="Standardised residuals v predictions"))
                                           ),
                                           fluidRow(
                                             plotOutput("resid")) #centre-left
                          )
                        ), #end well panel
                        
                        # OUTPUT DATA
                        wellPanel(
                          fluidRow(
                            column(4,helpText(h3("Output Data")))
                          ),
                          br(),
                          fluidRow(
                            helpText("Note: Interface must be opened in browser for downloads to be available - see option 'Open in Browser' at the top of the interface window."),
                            column(4,downloadButton("dlReps","Download replicates.csv")),
                            column(4,downloadButton("dlPL","Download probability limits.csv")),
                            column(4,downloadButton("dlSummary","Download summary .pdf"))
                          ),
                          br()
                        ) # end of wellPanel
                        
                      ) #end of vertical layout
                      
             ),  #end of simulation tab panel
             
             ##########################
             #FOURTH TAB PANEL: Help / About
             tabPanel("Help / About",
                      helpText(h3("Help")),
                      helpText(HTML("Please report any bugs, issues or feature requests to <a href='https://github.com/Jasenter/Probabilistic_App/issues'> https://github.com/Jasenter/Probabilistic_App/issues </a>")),
                      helpText(h3("Licenses, Warranty and Disclaimer")),
                      helpText(HTML("This open source project is provided under the <a href='https://github.com/Jasenter/Probabilistic_App/blob/master/LICENSE'> GPLv3 license</a>. Please see link for terms covering warranty, disclaimers, liability and use of this software.")),
                      helpText(h3("References")),
                      helpText(h4("Residual error model development")),
                      helpText(HTML("Hunter, J., Thyer, M., McInerney, D. & Kavetski, D. 2020. Achieving high-quality probabilistic predictions from hydrological models calibrated with a wide range of objective functions. Journal of Hydrology, (submitted).")),
                      helpText(HTML("McInerney, D., Kavetski, D., Thyer, M., Lerat, J. & Kuczera, G. 2019, Benefits of explicit treatment of zero flows in probabilistic hydrological modeling of ephemeral catchments. <i> Water Resources Research </i>, vol. 55, no. 12, pp 11035-11060, DOI: <a href = 'https://doi.org/10.1029/2018WR024148'> 10.1029/2018WR024148 </a>.")),
                      helpText(HTML("McInerney, D., Thyer, M., Kavetski, D., Bennett, B. Gibbs, M. & Kuczera, G. 2018. A simplified approach to produce probabilistic hydrological model predictions. <i> Environmental Modelling and Software </i>, DOI: <a href='https://doi.org/10.1016/j.envsoft.2018.07.001'> 10.1016/j.envsoft.2018.07.001 </a>.")),
                      helpText(HTML("McInerney, D., Thyer, M., Kavetski, D., Lerat, J. & Kuczera, G. 2017. Improving probabilistic prediction of daily streamflow by identifying Pareto optimal approaches for modeling heteroscedastic residual errors. <i> Water Resources Research </i>, vol. 53 no. 3, pp. 2199-2239, DOI: <a href='https://doi.org/10.1002/2016WR019168'> 10.1002/2016WR019168</a>.")),
                      helpText(HTML("Evin, G., Thyer, M., Kavetski, D., McInerney, D. & Kuczera, G. 2014. Comparison of joint versus postprocessor approaches for hydrological uncertainty estimation accounting for error autocorrelation and heteroscedasticity. <i> Water Resources Research </i>, vol. 50 no. 3, pp. 2350-2375,DOI: <a href='https://doi.org/10.1002/2013WR014185'> 10.1002/2013WR014185</a>.")),
                      helpText(HTML("Evin, G., Kavetski, D., Thyer, M. & Kuczera, G. 2013. Pitfalls and improvements in the joint inference of heteroscedasticity and autocorrelation in hydrological model calibration. <i> Water Resources Research </i>, vol. 49, no. 7, pp. 4518-4524, DOI: <a href ='https://doi.org/10.1002/wrcr.20284'> 10.1002/wrcr.20284 </a>")),
                      helpText(h4("Applications in streamflow forecasting")),
                      helpText(HTML("McInerney, D., Thyer, M., Kavetski, D., Laugesen, R., Tuteja, N. & Kuczera, G. 2020. Multi-temporal hydrological residual error modeling for seamless subseasonal streamflow forecasting. <i> Water Resources Research </i>, vol. 56, no. 11, pp. 2019WR026979, DOI: <a href = 'https://doi.org/10.1029/2019WR026979'> 10.1029/2019WR026979 </a>")),
                      helpText(HTML("Woldemeskel, F., McInerney D., Lerat J., Thyer M., Kavetski D., Shin D., Tuteja N. & Kuczera G. 2018. Evaluating post-processing approaches for monthly and seasonal streamflow forecasts. <i> Hydrology and Earth System Sciences </i>, vol. 22, no. 12, pp. 6257-6278, DOI: <a href = 'https://doi.org/10.5194/hess-22-6257-2018'> 10.5194/hess-22-6257-2018 </a>"))
             )
             
             
             
             
  ) #end of navbar layout
  
) #end of shinyui
