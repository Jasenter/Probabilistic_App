


####NOTE #####
#ON THIS SIDE OF THE APP (UI) WITHIN THE SHINYUI FUCTION LINES INSIDE FUNCTIONS (AND FUNCTIONS THEMSELVES) ARE SEPARATED USING COMMAS

# Define UI for dataset viewer application
shinyUI(
  
  navbarPage("Interactive Probabilistic Predictions",theme = shinytheme("united"),
             #FIRST TAB PANEL
             tabPanel("About",
                      helpText(h2("Interactive Probabilistic Predictions")),
                       helpText("This web-app produces probabilistic hydrological predictions using the LS-MoM method introduced in McInerney et al (2018)."),
                       helpText("The web-app assumes the user has already calibrated their hydrological model using their preferred software and an acceptable objective function (see below). This is referred to as 'Stage 1' of the calibration.") ,
                      helpText("The user should upload the observed and calibrated streamflow time series and the associated dates of measurement (daily timestep), and specify the Box Cox transformation parameters (lambda and A*) used in the objective function during Stage 1."),
                      helpText("The web-app will then estimate the residual error model parameters (referred to as 'Stage 2') and generate probabilistic predictions in the form of streamflow time series and associated 50% and 90% prediction limits. A selection of metrics and diagnostics from Evin et al (2014) and McInerney et al (2017) will be provided."),
                      helpText(h3("Objective functions")),
                      helpText("The web-app assumes a least-squares objective function, e.g. the sum-of-squared-errors (SSE) or equivalent Nash-Sutcliffe efficiency (NSE), computed from Box-Cox transformed flows (McInerney et al, 2017)."),
                      helpText("These include widely used objective functions such as the NSE (lambda=1, A*=0), the NSE on square-root transformed flows (lambda=0.5, A*=0) and the NSE on log-transformed flows (lambda=0, A*=0). "),
                      helpText(h3("Demonstration data")),
                      helpText("By default, loading up the web-app for the first time will display probabilistic streamflow predictions for the Yackandandah Creek catchment (Australia), obtained from the GR4J rainfall-runoff model pre-calibrated to the NSW-BC02 objective function."),
                      helpText(h3("Uploading your own data")),
                      helpText("To upload your own data, create a CSV file with three columns, 'date', obs' and 'pred'. Obs is a time series of observed data, pred is the corresponding time series of hydrological model predictions, and date is the daily timesteps in format (DD/MM/YYYY)."),
                      helpText(HTML("Download the demo data file to see the required format: <a href='http://www.algorithmik.org.au/dat/demoData.csv'> demo data file </a>.")),

                       helpText(h3("Further information")),
                      helpText(HTML("Further information on the importance probabilistic predictions in hydrology, and methods for   generating these predictions, can be found on the
                                    <a href='http://waterdecisions.org/reducing-hydrological-uncertainty/'> Intelligent Water Decisions Blog </a> and
                                    <a href='https://www.youtube.com/watch?v=mvuYlyF6S4s'> this talk</a> at the 2016 DEWNR NRM Science Conference. ")),


                       helpText(h3("Contact us")),
                       helpText(HTML("Contact <a href='mailto:jason.hunter@adelaide.edu.au'> Jason Hunter </a>
                                     for further details on how to use the web-app, the methods used for generating probabilistic predictions, and the diagnostics and performance metrics. ")),
                       helpText(HTML(" ")),
                       helpText(h3("References")),
                       helpText(HTML("Evin, G., Thyer, M., Kavetski, D., McInerney, D., & Kuczera, G. (2014). Comparison of joint versus postprocessor approaches for hydrological uncertainty estimation accounting for error autocorrelation and heteroscedasticity. Water Resources Research, 50(3), 2350-2375,
                                     <a href='https://doi.org/10.1002/2013WR014185'> DOI: 10.1002/2013WR014185</a>.")),
                      helpText(HTML("McInerney, D., Thyer, M., Kavetski, D., Bennett, B., Gibbs, M. & Kuczera, G. (2018). A simplified approach to produce probabilistic hydrological model predictions. Environmental Modelling and Software (submitted). ")),
                      helpText(HTML("McInerney, D., Thyer, M., Kavetski, D., Lerat, J., & Kuczera, G. (2017). Improving probabilistic prediction of daily streamflow by identifying Pareto optimal approaches for modeling heteroscedastic residual errors. Water Resources Research, 53(3), 2199-2239,
                                    <a href='https://doi.org/10.1002/2016WR019168'> DOI: 10.1002/2016WR019168</a>."))
                                         
                      
             ),     
              
             #SECOND TAB PANEL
             tabPanel("Simulation", icon=icon("area-chart","fa-1.9x"),  #adding an icon to the tab 
  
  
#            splitLayout(

              #Well panel (to group inputs)
#              fluidPage(
              
#              column(8,

#            ), # end of horizontal layout
  
            verticalLayout(
         
              wellPanel(  
                
                #Headings
                fluidRow(
                  column(4,helpText(h3("Input Data"))),
                  conditionalPanel(condition = "input.dataSel == 'Use demo data'",
                                   column(5,selectInput(inputId="model",label="demo data select",
                                                        choices =c("Yackandandah Creek (VIC)","Biggara (ACT)","Bethany (Barossa)","Lower Tanunda (Barossa)"),
                                                        selected="Yackandandah Creek (VIC)")))
                ),
                br(),
                #INPUT DATA                  
                fluidRow(  
                  column(5,radioButtons(inputId="dataSel",label="",choices=c("Use demo data","Load my own data"),selected="Use demo data",inline = TRUE))
                  ),
                fluidRow(
                  
                  #conditionalPanel(condition = "input.dataSel == 'Use demo data'", 
                  #                   column(2,selectInput("dataset","dataset",choices =c("B1 GR4J SLS"),selected="Gingera GR4J Log")) # Gingera GR4J Log
                  #),
                  conditionalPanel(condition = "input.dataSel == 'Load my own data'", 
                                   column(3,
                                          fileInput('file1',label=HTML("Load file (e.g. <a href='http://www.algorithmik.org.au/dat/demoData.csv'> demo data file </a>)"),
                                                    accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')), #,
                                          textInput(inputId="lab.date",label="header of dates"),
                                          textInput(inputId="lab.obs",label="header of observed data"),
                                          textInput(inputId="lab.pred",label="header of predicted data"),
                                          textInput(inputId="lab.unit",label="input units (e.g. mmd, m3/s, ML/d)"))
                                   )
                )
              ), #end of well panel              
              
              
# MODEL PARAMETERS               
              wellPanel(
                fluidRow(  
                  column(7,helpText(h3("Residual Model Parameters")))
                ),
                
                fluidRow(  
                  column(3,sliderInput("lambda","Transformation power parameter (Lambda)",min=0, max=1,value=0.2,step=0.1),
                  #column(3,
                         tags$head(tags$script(HTML(JScode))),
                         sliderInput("offset", "Transformation offset parameter [Dimensionless] (A*)",min = 0,max = 1e-0,value = 0.0001)),
                  column(2,selectInput(inputId="mean",label="mean parameter",choices=c("linear","constant","zero"),selected="zero")),
                  column(5,div(tableOutput("report"),style="font-size:120%"))
                ),
                
                fluidRow(
                 submitButton("Run simulation")
                )
              ),
 #expression(paste("Standardised residuals   ",bold(nu^"std"),sep=" "))
#WARNING MESSAGES 

              wellPanel(
                fluidRow(
                  column(7,helpText(h3("Warnings")))
                 ),
                  fluidRow(
                    column(7,textOutput(outputId="error1")),
                    column(7,textOutput(outputId="error2")),
                    column(7,textOutput(outputId="error3")),
                    column(7,textOutput(outputId="error4")),
                    column(7,textOutput(outputId="error5")),
                    column(7,textOutput(outputId="error6"))
                  )
              ),
 
#TIME SERIES PLOT  
              wellPanel(
                #Plot selector
                fluidRow( 
                  # column(4,helpText(h3("Plotting"))),
                  column(4,checkboxInput(inputId="plotTS",label="Plot timeseries",value=TRUE)) #tick box to hide plot & plotting options
                ),
                #Reveal date range selector if plotTS is ticked
                fluidRow( 
                  #column(2,checkboxInput(inputId="plotTS",label="Plot timeseries",value=TRUE)),
                  conditionalPanel(condition = "input.plotTS == true",  
                                   plotOutput("TS"),  #outputting a graphic beneath (option to turn off)
                                   column(8,sliderInput("datRange","X range",min=0, max=1000,value=c(50,450),step=5)),
                                   column(4,sliderInput("yRange","Y range",min=0, max=50,value=c(0,30),step=5))
                            )
                    )
              ),

#PREDICTIVE PERFORMANCE EVALUATION PLOTS              
              wellPanel( 
                fluidRow( 
                  column(4,checkboxInput(inputId="plotEval",label="Plot predictive performance evaluation",value=TRUE)) #tick box to hide plot & plotting options
                ),
                
                conditionalPanel(condition = "input.plotEval==true",
                 #Write Labels for Plot Selector,
                  fluidRow( 
                    column(6,helpText("Performance metric plot type")),
                    column(6,helpText("Performance benchmarking"))
                  ),
                  
                  #Plot selector dropdown menus (aligned with labels)
                  fluidRow( 

                    column(6,selectInput("perfPlot",NULL,choices =c("Predictive QQ plot"),selected="Predictive QQ plot")),
                    column(6,selectInput("boxPlot",NULL,choices = c("Reliability","Sharpness","Bias"),selected="Reliability"))
  
                  ),
                  
                  #then two side-by-side & perfPlot
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
#                  column(3,helpText("Transformed residuals with moving statistics"))
                  ),
                  fluidRow(
                    column(4,selectInput("resPlot",NULL,
                                         choices =c("Standardised residuals v predictions",
                                                    "Standardised residuals v cummulative probability of predictions",
                                                    "Probability density of standardised residuals",
                                                    "Transformed residuals",
                                                    "Transformed residuals with moving statistics",
                                                    "Autocorrelation plot of the residual innovations",
                                                    "Partial-autocorrelation plot of the residual innovations"),
                                         selected="Standardised residuals v predictions"))
                  #column(3,checkboxGroupInput(inputId="tranzPlot",label="",
                  #                            choices=c("mean","standard deviation","skewness","excess kurtosis","linear model"),
                  #                            inline=T,selected=NULL,width=NULL))
                  ),
                  fluidRow(
                    plotOutput("resid")) #centre-left
                #  column(3,plotOutput("tranz"))  #far-right
                )
              ), #end well panel

# OUTPUT DATA
              wellPanel(
                fluidRow(
                  column(4,helpText(h3("Output Data")))
                ),
                br(),
                fluidRow(
                  column(4,downloadButton("dlReps","Download replicates.csv")),
                  column(4,downloadButton("dlPL","Download probability limits.csv")),
                  column(4,downloadButton("dlSummary","Download summary .pdf"))
                ),
                br()
              ) # end of wellPanel

            ) #end of vertical layout
            
     )  #end of tab panel 2

   ) #end of navbar layout
) #end of shinyui
