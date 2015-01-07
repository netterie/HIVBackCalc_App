# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

# Source code (eventually load a package)
library(rootSolve)
library(ggplot2)
library(reshape)
library(Hmisc)
library(scales)
library(plyr)
library(xtable)
source('development/model.R')
source('development/other.R')

shinyServer(function(input, output, session) {

  ################################################## 
  # LOAD INPUT CSV
  ################################################## 
  # Access as rawdata()

  rawdata <- reactive({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })

  ################################################## 
  # VIEW INPUT CSV
  ################################################## 
  output$data_10rows <- renderTable({

    data <- rawdata()
    #data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    data[1:10,]
  })
  
  ################################################## 
  # SELECT SUBSET - NOT CODED YET
  ################################################## 
  
  dataf <- reactive({
    dataf <- rawdata()
  })

  ################################################## 
  # DESCRIBE SAMPLE
  ################################################## 

  output$describe_sample <- renderTable({

    dataf <- dataf()
    variables <- c(`Age Group`='agecat5', 
                   `Race/Ethnicity`='race', 
                   `Mode of Transmission`='mode')

    everHadNegTest_subgrouptab <- tabulate_everHadNegTest(dataf,
                                                      variables,
                                                      supercolumn=TRUE)

  },
     caption='Column % sums to 100 within each characteristic. Availability of testing history data within each subgroup level is shown as row percents of %Yes, %No, and %Missing',
     label='tab:sample',
     digits=0,
    table.placement='!h',
    caption.placement='top',
    include.rownames=FALSE,
    size='small',
    sanitize.text.function=function(str) { gsub('(\\.)*Percent(\\.)*', ' \\% ',str); }
  )

  ################################################## 
  # PLOT DIAGNOSES
  ################################################## 
  output$diagnoses_plot <- renderPlot({
      dataf <- dataf()
      plot_qtrDx(dataf)
  })

  ################################################## 
  # PLOT TESTING HISTORIES
  ################################################## 
  output$testinghistories_plot <- renderPlot({
      dataf <- dataf()
      everHadNegTest_time <- tabulate_everHadNegTest(dataf,'yearDx')
      plot_everHadNegTest(everHadNegTest_time)
  })

  ################################################## 
  # PLOT TID
  ################################################## 
  output$tid_plot <- renderPlot({
      dataf <- dataf()
      fig1combined(dataf, legendposition='right')
  })

  ################################################## 
  # RUN BACKCALCULATION
  ################################################## 
  results <- reactive({
    # Goal is to produces a list that contains everything
    # needed to get the stats object and summaries_both to work

    dataf <- dataf()

    ##### DEFINE DIAGNOSED COUNTS PER TIME UNIT (ASSUMED QTR, NOW)
    time_min <- min(dataf$timeDx)
    time_max <- max(dataf$timeDx)
    allTimes <- seq(time_min, time_max, by=0.25)
    obsCounts <- table(dataf$timeDx)
    allCounts <- structure(rep(0,length(allTimes)),
                           class='table',
                           names=allTimes)
    allCounts[names(allCounts)%in%names(obsCounts)] <- obsCounts

    ##### RUN BACKCALCULATION
    withProgress(message = 'Calculating, please wait', value=0, {

        all_noimpute <- runBackCalc(TID=dataf$infPeriod, 
                           impute=FALSE,
                           age=dataf$hdx_age,
                           diagnosedCounts=allCounts,
                           upperBound=FALSE, 
                           runBoth=TRUE,
                           intervalLength=0.25, 
                           printProgress=FALSE) 

        incProgress(detail='50% complete...')

        all_impute <- runBackCalc(TID=dataf$infPeriod, 
                           impute=TRUE,
                           age=dataf$hdx_age,
                           diagnosedCounts=allCounts,
                           upperBound=FALSE, 
                           runBoth=TRUE,
                           intervalLength=0.25, 
                           printProgress=FALSE) 

        summaries_noimpute <- summarize_runBackCalc(results=all_noimpute,
                                           diagnosedCounts=allCounts,
                                           times=allTimes)

        summaries_impute <- summarize_runBackCalc(results=all_impute,
                                           diagnosedCounts=allCounts,
                                           times=allTimes)

        summaries_both <- summarize_runBackCalc_combined(
                                    results=list(noimpute=all_noimpute, 
                                                 impute=all_impute), 
                                    diagnosedCounts=allCounts,
                                    times=allTimes)

        stats = data.frame(imputed=c(rep('Yes',
                                         nrow(summaries_impute[['stats']])),
                                     rep('No',
                                         nrow(summaries_noimpute[['stats']]))),
                           rbind(summaries_impute[['stats']],
                                 summaries_noimpute[['stats']]))

        stats <- format_stats(stats)

        return(list(summaries_both=summaries_both, stats=stats))

    }) # end withProgress

  }) # end reactive

  ################################################## 
  # PLOT BACKCALCULATION RESULTS
  ################################################## 
  output$results_plot <- renderPlot({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

     # Goal is to plot summaries_both object from run_main.R
     results <- results()
     results[['summaries_both']] 
  })

  ################################################## 
  # TABULATE BACKCALCULATION RESULTS
  ################################################## 
  output$results_table <- renderTable({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

     # Goal is to plot summaries_both object from run_main.R
     results <- results()
     results[['stats']]
  },
  label='tab:res_main',
  digits=0,
  size='small',
  include.rownames=FALSE
  )


})
