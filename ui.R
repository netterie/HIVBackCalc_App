library(shiny)

shinyUI(
  navbarPage('HIV Backcalculation',
             
             tabPanel('Select Data',
             
                 sidebarLayout(
                   fluid=FALSE,
                   sidebarPanel(
                     h5('Welcome'),
                     p('This app runs the HIV back-calculation model developed by Ian Fellows et al 
                       on a dataset of your choice.'),
                     h5('Data File'),
                     p('Select either the example dataset, or upload your own.'),
                     fileInput('file1', 'Choose CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv')),
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ','),
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"')                 
                   ),
                   mainPanel(
                     h5('File Contents'),
                     p('The first 10 rows are displayed to confirm the successful selection/
                       upload of your data. Please proceed to the "Data Summary" section next.'),
                     tableOutput('contents')
                   )
                 )
             ),
             tabPanel('Data Summary',
                tabsetPanel('Summary Tabs',
                tabPanel('Overview'),
                tabPanel('Diagnoses'),
                tabPanel('Testing Histories')
                )),
             tabPanel('TID'),
             tabPanel('Run Model'),
             tabPanel('Help')
             )

)