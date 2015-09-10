
library(shiny)

shinyUI(
  navbarPage('HIVBackCalc',
            tabPanel('Welcome',
                     p('Welcome to the Rshiny app for the HIVBackCalc R package! HIVBackCalc is a tool for the estimation of HIV incidence and undiagnosed cases. This app allows users to apply the methodology through an interactive interface.'),
                     p('Access our R package through Github', a('here.', href='https://github.com/hivbackcalc/package1.0/wiki')),
                     p('For methodological details see', a('Fellows IE, Morris M, Birnbaum JK, Dombrowski JC, Buskin S, Bennett A, et al. A New Method for Estimating the Number of Undiagnosed HIV Infected Based on HIV Testing History, with an Application to Men Who Have Sex with Men in Seattle/King County, WA. PLoS ONE. 2015 Jul 21;10(7):e0129551.', href='http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129551'))),
             tabPanel('Load Data',
             
                 sidebarLayout(
                   fluid=FALSE,
                   sidebarPanel(
                     h5('Welcome'),
                     p('This app runs the estimation of undiagnosed cases using a dataset of your choice.'),
                     h5('Data File'),
                     p('Select either the example dataset, or upload your own.'),
                     selectInput('data_choice', 'Choose data source',
                                 choices=c('MSM in King County, WA (simulated)',
                                           'Upload data')),
                     uiOutput('upload_data'),
                     br(), br(), br()
                   ),
                   mainPanel(
                     tabsetPanel('Data Panels',
                         tabPanel('Confirm Data',
					  column(width=7,	
                             h5('File Contents'),
                             p('The first 10 rows are displayed to confirm the successful selection/ upload of your data. Please proceed to the "Subgroup" sub-tab next.'),
                             tableOutput('data_10rows'))
                         ),
                         tabPanel('Optional: Subgroups',
                             h5('Optional: Choose a Subgroup'),
                             p('You may select a subgroup to analyze rather than using your entire sample - (comparisons of subgroups is not currently implemented). When you are done, or if you do not wish to analyze a subgroup, use the tabs at the top of the app to proceed to the "Examine Data" section.'),
                             uiOutput('svars_chosen'),
                             uiOutput('svars_values'),
                             p('Note: if you wish to analyze your full sample, choose "All."'),
                             # These line breaks increase the vertical length
                             # of the screen and helps avoid irritating 
                             # scrolling
                             br(), br(), br(), br(), br(), br(), br(), br(), br()

                         )
                     )
                   )
                 )
             ),
             tabPanel('Examine Data',
                tabsetPanel('Summary Tabs',
                tabPanel('Overview',
                   h5('Description of sample by age, race and mode of transmission'),
                   tableOutput('describe_sample')
                ),
                tabPanel('Diagnoses',
                   h5('Reported number of diagnosed cases over time',textOutput("label1")),
                   plotOutput('diagnoses_plot')
                ),
                tabPanel('Testing Histories',
                   h5('Testing history responses over time',textOutput("label2")),
                   p('When asked "Have you ever had a prior negative test?", cases could report "Yes", "No", or not answer the question'),
                   plotOutput('testinghistories_plot')
                )
                )),
             tabPanel('Calculate TID',
               h5('Time from infection to diagnosis (TID), for two cases:',textOutput("label3")),
               em('1. Base Case'), div('Missing testing history data are considered missing at random and are excluded from calculating the TID. The probability of infection is uniformly distributed between the time of last negative test and time of diagnosis.'),
               br(),
               em('2. Upper Bound'), div('Missing testing history data are considered missing at random and are excluded from calculating the TID. Infection is assumed to occur immediately following the date of last negative test, a worst case assumption.'),
               br(),
               plotOutput('tid_plot')
             ),
             tabPanel('Backcalculate Infections',
               sidebarLayout(
                  fluid=FALSE,
                  sidebarPanel(
                     h5('Click to backcalculate infections:',textOutput("label4")),
                     actionButton('go', label='Run backcalculation')
                  ),             
                  mainPanel(
                     # This accesses the stylesheet, which just sets a 
                     # location for the progress bar. Thanks to:
                     # https://groups.google.com/forum/#!topic/shiny-discuss/VzGkfPqLWkY 
                     # and https://github.com/johndharrison/Seed
                     tags$head(
                        tags$link(rel='stylesheet', type='text/css', href='styles.css'),
                        tags$script(type="text/javascript", src="busy.js")
                     ),
                     # Update 1/7/15: commented out in order to get withProgress() built into Shiny 0.10.2 to work
                     # Update 7/21/15: uncommented to try out with withProgress() 
                            
                     h5('Results'),
                     em('Note: counts refer to the time period for which diagnoses
                        were reported. In the KC data, this time period is a 
                        quarter-year.'),
                     br(),
                     br(),
                     p('The plots below shows the reported diagnoses over time with
                       the estimated incidence counts for the two TID cases 
                       (top panel). The bottom panel shows the estimated undiagnosed
                       counts over time for the two TID cases.'),
                       
                        div(class = "busy",
                                 p("Calculation in progress..."),
                                      img(src="ajax-loader.gif")
                                     ),

                     plotOutput('results_plot1'),
                     plotOutput('results_plot2'),
                     p('The table below summarizes reported diagnoses, estimated 
                       incidence for the two TID cases, and estimated undiagnosed
                       counts for the two TID cases across all time periods'),
                     tableOutput('results_table')
                  )
               )
             ),

            tabPanel('Help',
                     p('Please view our', a('instruction manual', href='https://rawgit.com/netterie/HIVBackCalc_App/master/Instruction%20Manual/Instruction_Manual_2.html'), 'for a guided tutorial.')
            )
             )
)

