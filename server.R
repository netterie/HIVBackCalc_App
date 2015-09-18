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
source('development/debug_fxns.R')

shinyServer(function(input, output, session) {

  ################################################## 
  # DATASET SELECTION
  ################################################## 

  output$upload_data <- renderUI({
      if (input$data_choice=='Upload data') {
        tagList(
            h5('Specify data features:'),
            checkboxInput('ehars', 'Is this raw eHARS output?', FALSE),
            checkboxInput('header', 'First row contains column names', TRUE),
            radioButtons('sep', 'Separator',
                      c(Comma=',',
                        Semicolon=';',
                        Tab='\t'),
                      ',', inline=TRUE),
            radioButtons('quote', 'Quote',
                      c(None='',
                        'Double Quote'='"',
                        'Single Quote'="'"),
                      '"', inline=TRUE),
            fileInput('file1', 'Choose file',
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv'))
        )
      } 
  })

  ################################################## 
  # LOAD AND DISPLAY DATA
  ################################################## 
  # Access as rawdata()

  rawdata <- reactive({

      # Note that stringsAsFactors=FALSE in read.csv to 
      # to facilitate subgroup selection later.

      switch(input$data_choice,
             'Upload data' = {
                # input$file1 will be NULL initially. After the user selects
                # and uploads a file, it will be a data frame with 'name',
                # 'size', 'type', and 'datapath' columns. The 'datapath'
                # column will contain the local filenames where the data can
                # be found.

                inFile <- input$file1
                
                if (is.null(inFile))
                  return(NULL)
                
                rawdata <- read.csv(inFile$datapath, header=input$header, 
                                   sep=input$sep, 
                                   quote=input$quote, stringsAsFactors=FALSE)
                if (input$ehars) rawdata <- format_eHARS(rawdata)$data
             },
             'MSM in King County, WA (simulated)' = {
                 rawdata <- read.csv('./development/data_KC_sim.csv', 
                                    header=TRUE, 
                                    stringsAsFactors=FALSE)
             }
      )
      return(rawdata)
  })

  output$data_10rows <- renderTable({

    data <- rawdata()
    #data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    data[1:10,]
  })
  
  ################################################## 
  # SELECT SUBSET 
  ################################################## 
  # Access as dataf()
  
  output$svars_chosen <- renderUI({

      svar_options <- colnames(rawdata())
      selectizeInput('svars_chooser', 'Select the variable that defines your subgroups:',
                    choices = c('All',svar_options),
                    options = list(placeholder = 'Select a variable below',
                                   onInitialize = I('function() { this.setValue(""); }')))
  })

  output$svars_values <- renderUI({
      if (!is.null(input$svars_chooser)) {
        if (input$svars_chooser!='' & input$svars_chooser!='All') {
          dataf <- rawdata()
          values <- unique(dataf[,input$svars_chooser])
          selectizeInput('svars_values_chooser', 'Select the subgroup:',
                    choices = values)
        }
      }
  })
  

  dataf <- reactive({
      if (is.null(input$svars_chooser)){ dataf<-rawdata()} 
			else if
	((input$svars_chooser=='') | (input$svars_chooser=='All')){
		dataf <- rawdata()}
		else {
          subset <- rawdata()[,input$svars_chooser]==input$svars_values_chooser
          dataf <- rawdata()[subset,]
      } 
  })

datalabel<-reactive({
  if (is.null(input$svars_chooser)) label<-("Subgroup = No Subgroup")
  else if (input$svars_chooser=="") label<-("Subgroup = No Subgroup")
  else if (input$svars_chooser=="All") label<-("Subgroup = No Subgroup")
  else label <-c("Subgroup = ", input$svars_values_chooser)
})

output$label1<-renderText({datalabel()})
output$label2<-renderText({datalabel()})
output$label3<-renderText({datalabel()})
output$label4<-renderText({datalabel()})

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
      #fig1combined(dataf, legendposition='right') - Removing Worst Case (Miss)
      fig1(dataf$infPeriod)
  })

  ################################################## 
  # DEBUG 1: VARIABLE INFO TABLES
  ################################################## 
  output$allRecords_table  <- renderTable({
      dataf <- dataf()
      datafInfo <- cbind(apply(dataf,2,function(x) sum(is.na(x))),
                         nrow(dataf))
      colnames(datafInfo) <- c('Missing', 'N')
      datafInfo <- transform(datafInfo,
                             PercMissing=round(100*Missing/N,0))
  })

  output$numericVar_table  <- renderTable({
    dataf <- dataf()

    summaries <- lapply(dataf, FUN=summary)
    numeric <- sapply(summaries, FUN=function(x) "Min."%in%names(x))
    summaries <- summaries[numeric]
    summaries <- lapply(summaries, function(x){
                        if (!"NA's"%in%names(x)) x <- c(x,0)
                        else x
                             })
    table <- do.call('rbind', summaries)
    colnames(table)[7] <- "NA's"
    return(table)
  })

  output$categoricalVar_table <- renderTable({
    dataf <- dataf()

    summaries <- lapply(dataf, FUN=summary)
    numeric <- sapply(summaries, FUN=function(x) "Min."%in%names(x))
    summaries <- summaries[!numeric]

    values <- sapply(names(summaries), function(x) 
                     paste(unique(dataf[,x]),collapse=','))
    table <- data.frame(Variable=names(values), 
                        Values=values,
                        stringsAsFactors=FALSE)
  },
  include.rownames=FALSE,
  display=c('s', 's', 's'))

  ################################################## 
  # DEBUG 2: DIAGNOSES
  ################################################## 
  diagnoses <- reactive({

    dataf <- dataf()
    time_min <- min(dataf$timeDx)
    time_max <- max(dataf$timeDx)
    allTimes <- seq(time_min, time_max, by=0.25)
    obsCounts <- table(dataf$timeDx)
    allCounts <- structure(rep(0,length(allTimes)),
                           class='table',
                           names=allTimes)
    allCounts[names(allCounts)%in%names(obsCounts)] <- obsCounts
    return(allCounts)
  })

  output$diagnoses_table <- renderTable({
    
    countsTable <- data.frame(Quarter=as.numeric(names(diagnoses())),
               Diagnoses=c(diagnoses()))
  },
  include.rownames=FALSE
  )

  ################################################## 
  # DEBUG 3: TID PDF
  ################################################## 
  pidList <- reactive({

    # Establish objects
    dataf <- dataf()
    allCounts <- diagnoses()

    TID=dataf$infPeriod
    TID_imputed=dataf$infPeriod
    age=dataf$hdx_age
    diagnosedCounts=allCounts
    intervalLength=0.25
    estType='base case'

    # TID PDF
    maxTime <- ceiling(max(TID, na.rm=TRUE)/0.25) + 1
    pid <- estimateProbDist(infPeriod=TID_imputed, 
                            intLength=intervalLength)
    return(list(pid=pid,maxTime=maxTime))
  })

  output$TIDPDF_table <- renderTable({

    pdf_dataframe <- data.frame(qtr=0:pidList()$maxTime, 
                                pdf=pidList()$pid(0:maxTime))

    colnames(pdf_dataframe) <- c('Quarters since infection',
                                 'Probability of diagnosis')

    round(pdf_dataframe,3)
  },
  include.rownames=FALSE,
  digits=3
  )

  ################################################## 
  # DEBUG 4: INCIDENCE
  ################################################## 
  allInc <- reactive({

    dataf <- dataf()
    diagnosedCounts <- diagnoses()
    pid <- pidList()$pid
    # Set y = nPrevInt NA's + number of diagnoses per quarter-year to indicate
    # that we want to backcalculate incidence for 100 time steps prior to 
    # our data
    nPrevInt <- 100
    y <- c(rep(NA,nPrevInt),diagnosedCounts)

    # estimateIncidence parameters
    gamma=.1
    tol=10^-4

    # estimateIncidence function with error catching
      lambda <- rep(mean(y,na.rm=TRUE),length(y))
      ll <- lambda
      dev <- Inf
      while(dev>tol){
        # Try to estimate
        trylambda <- tryCatch.W.E(meanEmUpdate(y,pid,lambda,gamma))
        # Only proceed if there are no errors
        if(is.null(trylambda$warning)) {
            lambda <- meanEmUpdate(y,pid,lambda,gamma)
            dev <- sum((ll-lambda)^2/ll)
            ll <- lambda
        } else {
            # Store lambda
            problemLambda <- lambda
            # Record the warning
            lambda <- trylambda$warning
            # Set dev=0 so the while loop will end
            dev=0
            # Break open the meanEmUpdate function
            recordProgress <- c('starting meanEmUpdate')
              T <- length(y)
              obs <- !is.na(y)
              a <- b <- c <- lamNew <- rep(NA,T)
              for(k in 1:length(problemLambda)){
                s <- 0:(T-k)
                b[k] <- sum(pid(s))
                no <- !obs[s+k]
                if(any(no))
                  a[k] <- sum(pid(s[no])) / b[k]
                else
                  a[k] <- 0
                c[k] <- 0
                for(d in s){
                  if(obs[k+d]){
                    c[k] <- c[k] + y[k+d]*pid(d) / sum(problemLambda[1:(k+d)]*pid(k+d-(1:(k+d))))
                  }
                }
              }
              if(gamma > 0){
                f <- function(ll){
                  (1/ll) * (a*b+c) * problemLambda - b - 2 * gamma * c(0, ll[2:T] - ll[-T]) - 
                    2 * gamma * c(ll[1:(T-1)] - ll[-1] ,0)
                }
                j <- function(ll){
                  diag <- (-1/ll^2)* (a*b+c)*problemLambda - 4 * gamma
                  diag[1] <- diag[1] + 2 * gamma
                  diag[T] <- diag[T] + 2 * gamma
                  off <- rep(0,T)
                  off[2:(T-1)] <- 2*gamma
                  rbind(-off,diag,off)
                }
                recordProgress <- c(recordProgress,
                                    'finding multiroot')
                # Run multiroot, which is modified to return some info
                # rather than an error if it does proceed into the 
                # error spot. lamNew is now all objects returned by
                # multiroot, rather than just the $root object
                lamNew <- multiroot(f=f,start=problemLambda,
                                    recordProgress=recordProgress,
                                    positive=TRUE,jacfunc=j,jactype="bandint")
              }else{
                lamNew <- problemLambda * (a + c / b)
              }
              lambda <- list(tryCatchError=lambda, 
                             multirootResult=lamNew, 
                             recordProgress=recordProgress) 
                }
      }
    # Return the typical list returned by estimateIncidence,
    # but with lambda as a warning message if the estimation
    # didn't work. 
      return(list(lambda=lambda,y=y,pid=pid,gamma=gamma,tol=tol))
  })

  output$printLambda <- renderPrint({
      allInc()$lambda
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
    #withProgress(message = 'Calculating, please wait', value=0, {

        all_noimpute <- runBackCalc(TID=dataf$infPeriod, 
                           impute=FALSE,
                           age=dataf$hdx_age,
                           diagnosedCounts=allCounts,
                           upperBound=FALSE, 
                           runBoth=TRUE,
                           intervalLength=0.25, 
                           printProgress=FALSE) 

        # The following if (impute) statements are used to remove
        # the Worst Case (Miss) scenario
        impute=FALSE
        
        if (impute) {

            #incProgress(detail='50% complete...')

            all_impute <- runBackCalc(TID=dataf$infPeriod, 
                               impute=TRUE,
                               age=dataf$hdx_age,
                               diagnosedCounts=allCounts,
                               upperBound=FALSE, 
                               runBoth=TRUE,
                               intervalLength=0.25, 
                               printProgress=FALSE) 
        }

        summaries_noimpute <- summarize_runBackCalc(results=all_noimpute,
                                           diagnosedCounts=allCounts,
                                           times=allTimes)

        if (impute) {
            summaries_impute <- summarize_runBackCalc(results=all_impute,
                                               diagnosedCounts=allCounts,
                                               times=allTimes)
        }

        if (impute) {
            summaries_both <- summarize_runBackCalc_combined(
                                        results=list(noimpute=all_noimpute, 
                                                     impute=all_impute), 
                                        diagnosedCounts=allCounts,
                                        times=allTimes)
        } else summaries_both <- summaries_noimpute

        if (impute) {
            stats = data.frame(imputed=c(rep('Yes',
                                             nrow(summaries_impute[['stats']])),
                                         rep('No',
                                             nrow(summaries_noimpute[['stats']]))),
                               rbind(summaries_impute[['stats']],
                                     summaries_noimpute[['stats']]))

            stats <- format_stats(stats)
        } else {
            stats <- summaries_noimpute[['stats']]
            colnames(stats)[1] <- 'Measure'
        }


        return(list(summaries_both=summaries_both, stats=stats))

    #}) # end withProgress

  }) # end reactive

  ################################################## 
  # PLOT BACKCALCULATION RESULTS
  ################################################## 
  output$results_plot1 <- renderPlot({
    
    # Don't display if backcalculation wasn't started
    if (input$go == 0) return()
    
    # Goal is to plot summaries_both object from run_main.R
    results <- results()
    results[['summaries_both']]$plotAll
  })
  output$results_plot2 <- renderPlot({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

     # Goal is to plot summaries_both object from run_main.R
     results <- results()
     results[['summaries_both']]$plotUndiag
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
