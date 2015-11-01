source("MNTExercise.R")


shinyUI(pageWithSidebar(
  
  headerPanel("MNT Quantification Exercixe"),
  
  sidebarPanel(
    selectInput("image_name", "Image Name", ls(img.names), selected="Cross")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Overview",includeMarkdown("overview.md")),
      tabPanel("Filtering", 
        selectInput('filter_name', 'Filter Function', ls(filter.funs),selected="None"),
        sliderInput('filter_size', 'Neighborhood Size', min=1, max=10,value=5,animate=T),
        sliderInput('filter_sigma', 'Sigma Value', min=0, max=10,value=3),
        #conditionalPanel(
        #  condition = "input.filter_name == 'Gaussian'",
        #  sliderInput('filter_sigma', 'Sigma Value', min=0, max=10,value=3)),
        plotOutput("filterPlot"),
        plotOutput("fhistPlot")
               
      ),
      tabPanel("Threshold", 
               sliderInput('threshold', 'Threshold Value', min=0, max=1,value=0.5,animate=T),
               checkboxInput("invertTrheshold", label = "Invert", value = FALSE),
               plotOutput("threshPlot"),
               plotOutput("thistPlot"),
               dataTableOutput("samplesummary")
               
      ),
      tabPanel("Metrics",
               plotOutput("binaryPlot"),
               #fluidRow(
                # column(1,
                 #       tableOutput("metrics")
                 #),
                 #column(10,
                #        tableOutput("metricsSummary")
                # )
               #)
               helpText("Mean and Standard Deviation of some metrices over all labeled objects:"),
               tableOutput("metricsSummary"),
               helpText("Detailed table of metrices for each individual object:"),
               tableOutput("metrics")
               
               
      ),
      tabPanel("Questions",includeMarkdown("questions.md"))
               
      )
    )
))
