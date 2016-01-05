packages <- c('ggplot2', 'reshape', 'tidyr2')



library(ggplot2)
library(reshape)
library(tidyr)

shinyUI(fluidPage(theme = 'bootstrap.css',
  headerPanel(img(src = "MindLab_logo-01.png", height = 100)),
  sidebarLayout(
    #Sidebar
    sidebarPanel(
      fluidRow(
        column(6,
               wellPanel(
                 fileInput('file1', label = 'Upload file'),
                 br(),
                 br()
               )),
        column(6,
               wellPanel(
                 uiOutput('MeasurementSelect'),
                 uiOutput('MetricSelect'))
        ))),
    
    #MainPanel
    mainPanel(
      #Significance Testing
      tabsetPanel(
        tabPanel('Significance Testing',
                 column(4,
                        'Ad 1',
                        wellPanel(
                          wellPanel(
                            uiOutput('AdSelect1'),
                            uiOutput('FocusSelect1')),
                          wellPanel(
                            plotOutput('plot1', height = '100px'),
                            br(),
                            br(),
                            verbatimTextOutput('descriptive1')),
                          wellPanel(
                            'Ad 1 vs Ad 2', 
                            verbatimTextOutput('test12'),
                            verbatimTextOutput('test13')
                          )
                        )
                 ),
                 
                 
                 column(4,
                        'Ad 2',
                        wellPanel(
                          wellPanel(
                            uiOutput('AdSelect2'),
                            uiOutput('FocusSelect2')),
                          wellPanel(
                            plotOutput('plot2', height = '100px'),
                            br(),
                            br(),
                            verbatimTextOutput('descriptive2')),
                          wellPanel(
                            verbatimTextOutput('test23')
                          )
                        )
                 ),
                 
                 column(4,
                        'Ad 3',
                        wellPanel(
                          wellPanel(
                            uiOutput('AdSelect3'),
                            uiOutput('FocusSelect3')),
                          wellPanel(
                            plotOutput('plot3', height = '100px'),
                            br(),
                            br(),
                            verbatimTextOutput('descriptive3')
                          )
                        )
                 )
        ),
     
      tabPanel('Overall view',
               tags$head(tags$style(type="text/css", "
                               #loadmessage {
                                    
                                    text-align: center;
                                    font-weight: bold;
                                    font-size: 100%;
                                    color: #FFFFFF;
                                    background-color: #003366;
                                    z-index: 105;
                                    }
                                    ")),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Loading...",id="loadmessage")),
               plotOutput('OverallView'),
               dataTableOutput('Overalltable')))
               
    ))))



