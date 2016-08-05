
library(shiny)

shinyUI(
  navbarPage('HIVBackCalc',
            tabPanel('Welcome',
                     p(strong('The app has moved to:')),
                     p('For local use via GitHub:', code("runGitHub('hivbackcalc/app', launch.browser=TRUE)")),
                     p('For online use:', a('https://hivbackcalc.shinyapps.io/main', 
                         href='https://hivbackcalc.shinyapps.io/main')),
                     p(strong('This version of the app is now a staging server for testing and debugging.'))),
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
                         tabPanel('Check Formatting',
                             h5('Presence of testing histories'),
                             p('Many years of diagnosis data that have very few diagnoses oand/or do not have testing histories may be problematic. In the selected dataset,'),
                             verbatimTextOutput('checkYearsWoutTH'),
                             h5('Responses to "Have you ever had a negative test?"'),
                             p('The allowed responses for this question, coded in the everHadNegTest variable, are TRUE, FALSE or NA. In the selected data,'),
                             verbatimTextOutput('checkEverHadNegTest'),
                             h5('Assumption for those with no prior testing history'),
                             p('The method requires that an assumption be applied to define a potential infection window for those who report never having had a prior testing history, i.e. everHadNegTest=FALSE. The "Age 16" assumption imputes a last negative test date at either age 16 or 18 years prior to diagnosis, whichever is more recent. In the selected data,'),
                             verbatimTextOutput('checkAssumptionNo'),
                             h5('Maximum infection window'),
                             p('The method caps the possible infection window at 18 years, given the natural history of HIV/AIDS. In the selected data,'),
                             verbatimTextOutput('checkMaxInfPeriod'),
                             h5('Formatting report'),
                             p('If you uploaded raw eHARS data, it was automatically formatted using the Age 16 assumption. The tables below summarize the raw data and the additional formatting assumptions applied. You may click the "Download formatted data" button to access the formatted data.'),
                             p('For all datasets, you may click the "Format Data" button to apply the selected assumption for those with no prior testing history. Additional formatting assumptions will be automatically applied as needed; for details, check the tables that will appear after the button is clicked.'),
                             br(),
                             uiOutput('formattingNumeric'),
                             uiOutput('formattingCategorical'),
                             uiOutput('formattingResults'),
                             downloadButton('downloadFormattedData', 
                                            'Download formatted data'),
                             radioButtons("assumptionNoChoice", 
                                          label = h6("Assumption for those with no prior testing history"), 
                                          choices = list("Age 16" = "age16"),
                                          selected = "age16"),
                             actionButton('applyFormatting', 'Format Data',
                                          class="btn-primary")
                         ),
                         tabPanel('Optional: Select Years',
                             br(),
                             p('Use the slider to select years of data to include in the analysis. Choose years for which the reporting of diagnoses has been completed and at least some testing history data are available'),
                             uiOutput('years_chosen')
                         ),
                         tabPanel('Optional: Subgroups',
                             h5('Optional: Choose a Subgroup'),
                             p('You may select a subgroup to analyze rather than using your entire sample - (comparisons of subgroups is not currently implemented). When you are done, or if you do not wish to analyze a subgroup, use the tabs at the top of the app to proceed to the "Examine Data" section.'),
                             uiOutput('svars_chosen'),
                             uiOutput('svars_values'),
                             uiOutput('svars_dispStrat'),
                             uiOutput('svars_strat'),
                             # These line breaks increase the vertical length
                             # of the screen and helps avoid irritating 
                             # scrolling
                             br(), br(), br(), br(), br(), br(), br(), br(), br()

                         ),
                         tabPanel('Optional: PLWH',
                            h5('Optional: Upload PLWH Data'),
                            p('HIVBackCalc estimates undiagnosed case counts. If you wish to estimate the undiagnosed fraction, please upload PLWH data for your area. Include a "Year" colum and a "Total" column indicating the estimate of diagnosed PLWH. Additional columns for subgroups are optional. If included, the subgroup column names must exactly match the coding for the subgroups in the testing history data.'),
                            p('If the MSM in King County dataset is selected in the left panel, an example PLWH dataset will be displayed below. If you select "Upload Data" on the left, you will see a prompt to upload PLWH below.'),
                     uiOutput('upload_plwh'),
                     tableOutput('plwh_view')
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
                   verbatimTextOutput('diagnoses_plot_coord'),
                   plotOutput('diagnoses_plot', click='plot_click'),
                   # h5('Tabular representation of number of diagnoses, by MSM vs non-MSM'),
                   tableOutput('dx_samplesize')
                ),
                tabPanel('Testing Histories',
                   h5('Testing history responses over time',textOutput("label2")),
                   p('When asked "Have you ever had a prior negative test?", cases could report "Yes", "No", or not answer the question'),
                   verbatimTextOutput('testinghistories_plot_coord'),
                   plotOutput('testinghistories_plot', click='plot_click')
                )
                )),
             tabPanel('Calculate TID',
               h5('Time from infection to diagnosis (TID)',textOutput("label3")),
               h5('Two Cases:'),
               em('1. Base Case'), div('Missing testing history data are considered missing at random and are excluded from calculating the TID. The probability of infection is uniformly distributed between the time of last negative test and time of diagnosis.'),
               br(),
               em('2. Upper Bound'), div('Missing testing history data are considered missing at random and are excluded from calculating the TID. Infection is assumed to occur immediately following the date of last negative test, a worst case assumption.'),
               br(),
               #verbatimTextOutput('tid_plot_coord'),
               plotOutput('tid_plot', click='plot_click'),
               tableOutput('TIDPDF_table')
             ),
             tabPanel('Debug Info',
                tabsetPanel('Debug Tabs',
                tabPanel('Variable Summaries',
                   h5('All records'),
                   tableOutput('allRecords_table'),
                   h5('Numeric variables'),
                   tableOutput('numericVar_table'),
                   h5('Categorical variables'),
                   tableOutput('categoricalVar_table')
                ),
                tabPanel('Diagnoses',
                    h5('Diagnoses by quarter'),
                    tableOutput('diagnoses_table')
                ),
                tabPanel('TID PDF',
                    h5('Probability of diagnosis by quarters since infection')#,
              #      tableOutput('TIDPDF_table')
                ),
                tabPanel('Incidence',
                    h5('Value of lambda (incidence vector) - may take several minutes to compute:'),
                    verbatimTextOutput('printLambda')
                )
             )),
             tabPanel('Backcalculate Infections',
               sidebarLayout(
                  fluid=FALSE,
                  sidebarPanel(
                     h5('Click to backcalculate infections:',textOutput("label4")),
                     actionButton('go', label='Run backcalculation'),
                     h5('Download analysis report as a Word, HTML or PDF file:'),
                     downloadButton('downloadReportWord', label='Word'),
                     downloadButton('downloadReportHTML', label='HTML'),
                     downloadButton('downloadReportPDF', label='PDF')
                  ),             
                  mainPanel(
                     tabsetPanel('Data Panels',
                     tabPanel('Time Step',
                              h5('Select a time step'),
                              p('The time step refers to the interval by which diagnosis dates are identified. The time step influences the sample size, as longer time steps will accumulate more diagnoses, as well as the interpretation of results, as it changes the definition of "undiagnosed." For example, 25 diagnoses per quarter = 100 diagnoses per year, and someone who is infected in Q1 but diagnosed in Q4 is undiagnosed for 3 quarters but still diagnosed in the same year in which they were infected. The default time step is a quarter-year (0.25).'),
                              p('Use the following diagnostics to determine your selection:'),
                              h6('1. Current time step'),
                              verbatimTextOutput('selectedtimestep'),
                              h6('2. Smallest time step allowed by data (see variable timeDx)'),
                              verbatimTextOutput('datatimestep'),
                              h6('3. Number of time steps having 0-4, 5-19, 20-49, and 50-999 diagnoses, by selected stratum variable'),
                              verbatimTextOutput('avgdxbytimestep'),
                              h5('Time step selection'),
                              p('WARNING: if you aggregate to a 0.5-year or 1-year step, this can introduce a slight downward bias in early years and upward bias in the most recent 3-5 years, by as much as +/- 10% of cases when compared to the quarterly estimates. This translates into a smaller impact on the undiagnosed fraction. For example, in a population with 15,000 PLWH of which 10% are undiagnosed, a +10% bias in undiagnosed cases will raise the undiagnosed fraction to 11%.'),
                              radioButtons("timestepChoice", 
                                          label = h6("Select a time step"), 
                                          choices = list("Quarter-Year" = 0.25,
                                                         "Half-Year" = 0.5,
                                                         "Year" = 1),
                                          selected = 0.25)
                              ),
                     tabPanel('Results',
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

                     # verbatimTextOutput('results_plot_coord'),
                     plotOutput('results_plot', click='plot_click'),
                     p('The table below summarizes reported diagnoses, estimated 
                       incidence for the two TID cases, and estimated undiagnosed
                       counts for the two TID cases across all time periods. Click 
                       the download button to save detailed results by year.'),
                     tableOutput('results_table'),
                     downloadButton('downloadResultsByYear', 
                                    'Download results table'),
                     br(), br(), 
                     p('If you provided PLWH data, the chart below shows undiagosed counts (top panel) and true prevalence, the sum of undiagnosed counts and the diagnosed PLWH estimates (middle panel). The percent undiagnosed is computed as undiagnosed counts divided by true prevalence (bottom panel). The bars show the ranges between the Base Case and Upper Bound estimates.'),
                     plotOutput('results_trueprevplot'),
                     p('The plot displays the same data in an alternate format. For each of the Base Case (left) and Upper Bound (right), the bars show the breakdown of total true PLWH prevalence into diagnosed and undiagnosed cases.'),
                     plotOutput('results_trueprevplot2')
                  )
                  )
                  ) # end main panel
               )
             ),

            tabPanel('Help',
                     p('Please view our', a('instruction manual', href='https://rawgit.com/hivbackcalc/app/master/manual/app_manual.html'), 'for a guided tutorial.')
            )
             )
)

