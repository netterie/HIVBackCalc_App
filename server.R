# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

# Source code
source('development/other.R')
library(HIVBackCalc)
library(reshape2)
#library(reshape)
#library(Hmisc)
#library(scales)
library(xtable)

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
  # rawdataList() is the same as rawdata() unless
  # input$ehars=TRUE, in which case there is a 
  # $data element along with formatting report elements

  rawdataList <- reactive({

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
                
                # This will be a data frame
                rawdata <- read.csv(inFile$datapath, header=input$header, 
                                   sep=input$sep, 
                                   quote=input$quote, stringsAsFactors=FALSE)
                # This will be a list
                if (input$ehars) rawdata <- format_data(rawdata, eHARS=TRUE)
             },
             'MSM in King County, WA (simulated)' = {
                # This will be a data frame
                 rawdata <- read.csv('./development/data_KC_sim.csv', 
                                    header=TRUE, 
                                    stringsAsFactors=FALSE)
             }
      )
      return(rawdata)
  })

  # Extract just the data frame
  rawdata <- reactive({
      if (!is.null(rawdataList())) {
          if (!is.data.frame(rawdataList())) {
              return(rawdataList()$data)
          } else return(rawdataList())
      } else return(NULL)
  })

  # Display first 10 rows
  output$data_10rows <- renderTable({

    data <- rawdata()
    #data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    data[1:10,]
  })
  
  ################################################## 
  # CHECK DATA
  ################################################## 
  # Checks the formattedData() object, which is 
  # just rawdata() until the Format button is clicked
  # (see next section)

    checkData <- reactive({
        return(check_data(formattedData()))
    })

    output$checkYearsWoutTH <- renderPrint({
        print(checkData()[["Years with sparse diagnoses"]])
        print(checkData()[["Years without testing histories"]])
    })
    output$checkEverHadNegTest <-  renderPrint({
        print(checkData()[["everHadNegTest levels"]])
    })
    output$checkAssumptionNo <- renderPrint({
        print(checkData()[["Assumption for everHadNegTest=FALSE"]][1])
    })
    output$checkMaxInfPeriod <- renderPrint({
        print(checkData()[["Maximum infPeriod"]])
    })

  ################################################## 
  # FORMAT DATA (beyond format_eHARS())
  ################################################## 

    # Format formattedData upon click of the format button
    # This allows the button to be clicked more than once, with 
    # a new formatting choice selected and applied
    formattedDataList <- reactive({
        if (input$applyFormatting==0 & is.data.frame(rawdataList())) {
            # No button click, no eHARS formatting applied
            return(list(data=rawdata(),
                        rawdata=rawdata()))
        } else if (input$applyFormatting==0 & !is.data.frame(rawdataList())) {
            # No button click, eHARS formatting applied
            # For convenience in resetFormattedData below, don't
            # store the raw eHARS data in formattedData$rawdata, 
            # just store the formatted data
            return(list(data=rawdata(),
                        rawdata=rawdata(),
                        assumptions=rawdataList()$assumptions, 
                        checkEverHad=rawdataList()$checkEverHad,
                        rawVarSum=rawdataList()$rawVarSum))
        } else {
            # Button is clicked
            isolate({
                fdataList <- format_data(rawdata(), eHARS=FALSE,
                                     assumptionNo=input$assumptionNoChoice)
                return(fdataList)
            })
        }
    })

    # Reset status: check raw data dimensions against pre-formatted data
    resetFormattedData <- reactive({
        # Same number of rows
        sameN <- nrow(rawdata())==nrow(formattedDataList()$rawdata)
        # Same original columns (this is less meaningful)
        sameP <- sum(colnames(rawdata())%in%
                     colnames(formattedDataList()$rawdata))==ncol(rawdata())
        reset <- ifelse(sameN & sameP, FALSE, TRUE)
        return(reset)
    })
    
    
    # Either return the raw data or, if the formatting 
    # button was clicked, the formatted data
    formattedData <- reactive({
        if (input$applyFormatting==0 | resetFormattedData()) {
            return(rawdata())
        } else return(formattedDataList()$data)
    })

    # Download of formatted data: works as long as data
    # have NOT just been reset with a new dataset
      output$downloadFormattedData <- downloadHandler( 
        filename=function() {
            validate(need(!is.null(formattedData()),
                     'Results not ready'))
            'testingHistories_formatted.csv'
        },
        content = function(file) { 
            write.csv(formattedData(), file) 
        }
      )

    # Summary of variables in the raw data and formatting assumptions,
    # compiled during formatting. Display if Format button
    # was clicked or input data were eHARS and 
    # a new dataset hasn't just been selected
    output$formattingNumeric <- renderTable({
      if ((input$applyFormatting!=0 & !resetFormattedData()) |
          (input$applyFormatting==0 & !resetFormattedData() & 
           !is.data.frame(rawdataList()))) {
          return(formattedDataList()$rawVarSum$Numeric)
      } else return(NULL)
    },
     caption='Summary of numeric variables in the data, pre-formatting',
     label='tab:format2',
     digits=0,
    caption.placement='top',
    include.rownames=TRUE,
    size='small'
    )

    output$formattingCategorical <- renderTable({
      if ((input$applyFormatting!=0 & !resetFormattedData()) |
          (input$applyFormatting==0 & !resetFormattedData() & 
           !is.data.frame(rawdataList()))) {
          return(formattedDataList()$rawVarSum$Categorical)
      } else return(NULL)
    },
     caption='Summary of non-numeric variables in the data, pre-formatting. The display of values is truncated after 25 unique values.',
     label='tab:format2',
     digits=0,
    caption.placement='top',
    include.rownames=FALSE,
    size='small'
    )

    output$formattingResults <- renderTable({
      if ((input$applyFormatting!=0 & !resetFormattedData()) |
          (input$applyFormatting==0 & !resetFormattedData() & 
           !is.data.frame(rawdataList()))) {
          return(formattedDataList()$assumptions)
      } else return(NULL)
    },
     caption='Formatting assumptions applied to the data, in addition to the assumption selected for those with no testing history. Missing month/day and illogical last negative are relevant only when the input data are raw eHARS data.',
     label='tab:format1',
     digits=0,
    caption.placement='top',
    include.rownames=FALSE,
    size='small'
    )

  ################################################## 
  # SELECT SUBSET
  ################################################## 
  # Access as dataf()
  
  # Years
  output$years_chosen <- renderUI({
      years <- floor(formattedData()$timeDx)
      sliderInput('selectedYears', 'Select Years:',
                  min=min(years), max=max(years),
                  value=c(min(years), max(years)),
                  step=1,
                  sep='')
  })

  # Other groups
  # Defaults will be svars_chooser='All' and svars_strat='mode2'
  output$svars_chosen <- renderUI({

      svar_options <- colnames(formattedData())
      selectizeInput('svars_chooser', 
                     'Select the variable that defines your subgroups:',
                    choices = c('All',svar_options),
                    selected = 'All',
                    options = list(create=TRUE))
#                    options = list(placeholder = 'Select a variable below',
#                                   onInitialize = I('function() { this.setValue(""); }')))
  })

      output$svars_values <- renderUI({
          if (!is.null(input$svars_chooser)) {
            if (input$svars_chooser!='' & input$svars_chooser!='All') {
              dataf <- formattedData()
              values <- unique(dataf[,input$svars_chooser])
              selectizeInput('svars_values_chooser', 'Select the subgroup:',
                        choices = values)
            }
          }
      })

      output$svars_dispStrat <- renderUI({
          if (!is.null(input$svars_chooser)) {
          if (input$svars_chooser=='All') {
              list(
                p('Note: if you wish to analyze your full sample, choose "All." The default procedure for analyzing the whole sample is to stratify by MSM vs non-MSM using the "mode2" variable.'),
              actionButton('changeStrat', 'Change stratification variable'))
          }

          }
      })

      output$svars_strat <- renderUI({
          if (!is.null(input$changeStrat)) {
          if (input$changeStrat!=0) {
              isolate(
              selectizeInput('svars_values_strat', 
                             'For All, select stratification variable (default is mode2):',
                             choices = c('None', 'mode2'),
                             selected='mode2',
                             options = list(create=TRUE))
              )
            }
          }
      })

      dataf <- reactive({
          # Subgroups
          if (is.null(input$svars_chooser)){ 
              dataf<-formattedData()
          } else if ((input$svars_chooser=='') | (input$svars_chooser=='All')) {
              dataf <- formattedData()
          } else {
              subset <- formattedData()[,input$svars_chooser]==input$svars_values_chooser
              dataf <- formattedData()[subset,]
          } 
          # Years
          if (!is.null(input$selectedYears)) {
              dataf <- subset(dataf,timeDx>=input$selectedYears[1] &
                                    timeDx<(input$selectedYears[2]+1))
          }
          return(dataf)
      })

    subgroupVar <- reactive({
        ifelse(is.null(input$svars_chooser), 'All', 
               ifelse(input$svars_chooser=='All', 'All',
                      input$svars_values_chooser))
    })

    # For now, stratification is not allowed for subgroups 
    stratVar <- reactive({
        ifelse(!is.null(subgroupVar()),
               ifelse(subgroupVar()=='All',
                      ifelse(is.null(input$svars_values_strat), 'mode2',
                             input$svars_values_strat), input$svars_values_strat),
               'None')
    })

    datalabel<-reactive({
        subgrouplab <- paste0('Subgroup = ', subgroupVar())
        finallab <- paste(subgrouplab,
                          ifelse(stratVar()=='None', 'No Strata',
                                 paste('Stratified by', stratVar())),
                          sep='; ')
    })

    output$label1<-renderText({datalabel()})
    output$label2<-renderText({datalabel()})
    output$label3<-renderText({datalabel()})
    output$label4<-renderText({datalabel()})

  ################################################## 
  # LOAD PLWH
  ################################################## 
  output$upload_plwh <- renderUI({
      if (input$data_choice=='Upload data') {
        tagList(
            h5('Upload PLWH file:'),
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
            fileInput('filePLWH', 'Choose file',
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv'))
        )
      }
  })

  plwh <- reactive({

      # Note that stringsAsFactors=FALSE in read.csv to 
      # to facilitate subgroup selection later.

      switch(input$data_choice,
             'Upload data' = {
                # input$file1 will be NULL initially. After the user selects
                # and uploads a file, it will be a data frame with 'name',
                # 'size', 'type', and 'datapath' columns. The 'datapath'
                # column will contain the local filenames where the data can
                # be found.

                inFile <- input$filePLWH
                
                if (is.null(inFile))
                  return(NULL)
                
                plwh <- read.csv(inFile$datapath, header=TRUE,
                                   sep=input$sep, 
                                   quote=input$quote, stringsAsFactors=FALSE)
             },
             'MSM in King County, WA (simulated)' = {
                 plwh <- read.csv('./development/plwh_KC.csv', 
                                    header=TRUE, 
                                    stringsAsFactors=FALSE)
             }
      )
      if (!is.null(input$selectedYears)) {
          plwh <- subset(plwh, Year>=input$selectedYears[1] &
                               Year<(input$selectedYears[2]+1))
      }
      return(plwh)
  })

  output$plwh_view <- renderTable({

    data <- plwh()
    
    data
  })
  ################################################## 
  # DESCRIBE SAMPLE
  ################################################## 

  output$describe_sample <- renderTable({

    dataf <- dataf()
    variables <- c(`Age Group`='agecat5', 
                   `Race/Ethnicity`='race', 
                   `Mode of Transmission`='mode')

    everHadNegTest_subgrouptab <- tabTestHist(dataf, variables, 
                                              supercolumn=TRUE,
                                              fullsample_row=TRUE)

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
      plotDiagnoses(dataf)
  })
  output$diagnoses_plot_coord <- renderText({
      paste0('x=', input$plot_click$x, 
             '\ny=', input$plot_click$y)
  })

  output$dx_samplesize <- renderTable({

      dataf <- dataf()
      if (is.null(stratVar())) {
          table(dataf$timeDx)
      } else if (stratVar()=='None') {
          table(dataf$timeDx)
      } else {
          table(dataf[,stratVar()], dataf$timeDx)
      }

  },
     caption='Diagnoses per time step',
     label='tab:dxByMSM',
     digits=0,
    table.placement='!h',
    caption.placement='top',
    include.rownames=TRUE,
    size='small',
    sanitize.text.function=function(str) { gsub('(\\.)*Percent(\\.)*', ' \\% ',str); }
  )

  ################################################## 
  # PLOT TESTING HISTORIES
  ################################################## 
  output$testinghistories_plot <- renderPlot({
      dataf <- dataf()
      plotTestHist(dataf)
  })
  output$testinghistories_plot_coord <- renderText({
      paste0('x=', input$plot_click$x, 
             '\ny=', input$plot_click$y)
  })

  ################################################## 
  # ESTIMATE AND PLOT TID
  ################################################## 
  diagInterval = 0.25
  TIDs <- reactive({
      dataf <- dataf()
      return(estimateTID(dataf$infPeriod, intLength=diagInterval))
  })

  output$tid_plot <- renderPlot({
    plot(TIDs(), intLength=diagInterval, 
         cases = c('Base Case', 'Upper Bound'))
  })
  output$tid_plot_coord <- renderText({
      paste0('x=', input$plot_click$x, 
             '\ny=', input$plot_click$y)
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
    allTimes <- seq(time_min, time_max, by=diagInterval)
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

  output$TIDPDF_table <- renderTable({

    pid <- TIDs()$base_case$cdf
    pidU <- TIDs()$upper_bound$cdf

    # Survivor fxn by quarter-year
    pdf_dataframe <- data.frame(yrs=c(0, 1:length(pid)/4),
                                surv=c(1, 1-pid),
                                survU=c(1, 1-pidU))
    # Focus on half-years, otherwise there's too much info
    pdf_dataframe <- pdf_dataframe[seq(1,nrow(pdf_dataframe),by=2),]

    colnames(pdf_dataframe) <- c('Years since infection',
                                 'Base Case fraction still undiagnosed',
                                 'Upper Bound fraction still undiagnosed')

    round(pdf_dataframe,2)
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
    pid <- TIDs()$base_case$pdffxn
    # Set y = nPrevInt NA's + number of diagnoses per quarter-year to indicate
    # that we want to backcalculate incidence for 100 time steps prior to 
    # our data
    nPrevInt <- 100
    y <- c(rep(NA,nPrevInt),diagnosedCounts)

    # Error handler
    tryCatch.W.E <- function(expr) { 
        W <- NULL
        w.handler <- function(w) { # warning handler 
            W <<- w
            invokeRestart("muffleWarning")
        }
        list(value = withCallingHandlers(tryCatch(expr, 
                                                  error = function(e) e), 
                                         warning = w.handler), warning = W)
    }

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
    TIDs <- TIDs()

    # Stratified results
    if (subgroupVar()=='All' & stratVar()!='None') {
        stratResults <- runSubgroups(testhist=dataf,
                                     subvar='mode2',
                                     intLength=diagInterval)
        return(stratResults[['Total-stratified']]$results)
    } else {
        # Not stratified
        allResults <- runBackCalc(testhist=dataf,
                                  intLength=0.25)

        return(allResults$results)
    }

    if (1==0) {
        diagCounts = tabulateDiagnoses(dataf, intLength=diagInterval)
        incidenceBase = estimateIncidence(y=diagCounts,
                                        pid=TIDs[['base_case']]$pdffxn,
                                        gamma=0.1,
                                        verbose=FALSE)
        incidenceUpper = estimateIncidence(y=diagCounts,
                                        pid=TIDs[['upper_bound']]$pdffxn,
                                        gamma=0.1,
                                        verbose=FALSE)
        undiagnosedBase <- estimateUndiagnosed(incidenceBase)
        undiagnosedUpper <- estimateUndiagnosed(incidenceUpper)
        results <- combineResults(list(`Base Case`=list(incidenceBase,
                                                  undiagnosedBase),
                                   `Upper Bound`=list(incidenceUpper,
                                                    undiagnosedUpper)))

        return(results)
    }

  }) # end reactive

  #output$calcDone <- reactive({
  #    return(!is.null(results()))
  #})
  #outputOptions(output, 'calcDone', suspendWhenHidden=FALSE)

  ################################################## 
  # PLOT BACKCALCULATION RESULTS
  ################################################## 
  output$results_plot <- renderPlot({
    
    # Don't display if backcalculation wasn't started
    if (input$go == 0) return()
    
    # Goal is to plot summaries_both object from run_main.R
    results <- results()
    plot(results)
  })
  output$results_plot_coord <- renderText({
      paste0('x=', input$plot_click$x, 
             '\ny=', input$plot_click$y)
  })

  ################################################## 
  # TRUE PREVALENCE
  ################################################## 
  trueprev <- reactive({
     # Don't attempt if backcalculation wasn't started
     if (input$go == 0) return()

     plwh <- plwh()

      if (is.null(plwh)) return(NULL)

      group <- ifelse(is.null(input$svars_chooser), 'Total',
                      ifelse(input$svars_chooser=='' |
                             input$svars_chooser=='All', 'Total',
                             input$svars_values_chooser))
      if (!group%in%colnames(plwh)) stop('Incorrect column 
                                         names for PLWh data')

      trueprev <- calcTruePrev(results(), 
                               plwh[,c('Year', group)])

      # First plot
      tpMean <- subset(trueprev, 
                       Estimate!='PLWHA',
                       select=c('Year', 'Diagnoses/Case',
                                'Estimate', 'Mean'))
      tpMean <- melt(tpMean,id.vars=c('Year', 'Estimate', 'Diagnoses/Case'))
      tpMeanW <- dcast(tpMean, Year+Estimate~`Diagnoses/Case`)

    trueprevplot <- ggplot(tpMeanW,
                 aes(x=as.factor(Year), y=`Base Case`,
                     ymin=`Base Case`,
                     ymax=`Upper Bound`,
                     colour=Estimate,
                     fill=Estimate)) +
          scale_x_discrete(limits=c(as.character(min(tpMeanW$Year):
                                    max(tpMeanW$Year))), name='') +
          scale_y_continuous(name='') + 
          scale_colour_manual(name='', 
                            values=c('#e0f3db', '#a8ddb5', '#43a2ca')) + 
          scale_fill_manual(name='', 
                            values=c('#e0f3db', '#a8ddb5', '#43a2ca')) + 
          guides(colour=FALSE, fill=FALSE) + 
          geom_crossbar(width=0.5, position=position_dodge(width=1.05)) +
          theme_bw() +
          facet_grid(Estimate~., scales='free_y') # facet_wrap won't work

     # Second plot
        tp <- subset(trueprev, Estimate=='PLWHA' | Estimate=='Undiagnosed Cases', 
                     select=c('Year', 'Diagnoses/Case', 'Estimate', 'Mean'))
        colnames(tp)[which(colnames(tp)=='Diagnoses/Case')] <- 'Case'
        plwh <- subset(tp, Estimate=='PLWHA')
        tp2 <- rbind(subset(tp, Estimate!='PLWHA'),
                     data.frame(subset(plwh, select=c('Year', 'Mean', 'Estimate')),
                                Case='Base Case'),
                     data.frame(subset(plwh, select=c('Year', 'Mean', 'Estimate')),
                                Case='Upper Bound'))
        tp2$Estimate <- factor(tp2$Estimate, levels=c('PLWHA', 'Undiagnosed Cases'),
                               labels=c('Diagnosed PLWHA', 'Undiagnosed Cases'))
        tp2 = arrange(tp2, Year, Estimate)

        tp3 = ddply(tp2, .(Year, Case), transform, Percent = Mean/sum(Mean) * 100)

        tp3 = ddply(tp3, .(Year, Case), transform, pos = (cumsum(Mean) - 0.5 * Mean))
        tp3$label = paste0(sprintf("%.0f", tp3$Percent), "%")


        trueprevplot2 <- ggplot(tp3, 
                                aes(x = factor(Year), y = Mean, fill = Estimate)) +
           geom_bar(stat = "identity", width = .7) +
              geom_text(aes(y = pos, label = label), size = 4) +
                 coord_flip() + 
                 facet_grid(.~Case) + theme_bw() + 
                 scale_x_discrete(name='') + 
                 scale_y_continuous(name='Number of Cases') + 
                 scale_fill_manual(name='', values=c('#a8ddb5', '#43a2ca')) + 
                 theme_bw() + 
                 theme(legend.position='bottom') +
                 theme(axis.text = element_text(size = 12))

     return(list(table=trueprev, plot=trueprevplot,
                 plot2=trueprevplot2))
  })

  ################################################## 
  # TABULATE BACKCALCULATION RESULTS
  ################################################## 
  output$results_table <- renderTable({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

     # Goal is to plot summaries_both object from run_main.R
     results <- results()
     results$resultsSummary
  },
  label='tab:res_main',
  digits=0,
  size='small',
  include.rownames=FALSE
  )

  tableToDownload <- reactive({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

     if (is.null(trueprev())) return(results()$resultsSummaryYear)
     else return(trueprev()$table)
  })

  output$downloadResultsByYear <- downloadHandler( 
    filename=function() {
        validate(need(!is.null(results()),
                 'Results not ready'))
        'resultsByYear.csv'
    },
    content = function(file) { 
        write.csv(tableToDownload(), file) 
    }
  )

  output$results_trueprevplot <- renderPlot({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

      if (!is.null(trueprev())) trueprev()$plot
      else(NULL)
  })

  output$results_trueprevplot2 <- renderPlot({

     # Don't display if backcalculation wasn't started
     if (input$go == 0) return()

      if (!is.null(trueprev())) trueprev()$plot2
      else(NULL)
  })

})



