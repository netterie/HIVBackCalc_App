########################
# debug_run.R
# 
# Purpose: 
# Run back-calculation using the modified fxns in
# debug_fxns.R in order to debug. Results should
# inform how to modify server.R and ui.R in the
# dev version of the app for remote debugging.
#
# Dependencies:
# None
#
# History: 
#
# Notes:
# Begun 9/10/15 by JKB
########################

#############################################################
# SETUP
#############################################################
rm(list=ls())
# TEMPORARY: SOURCE FUNCTIONS
source('/Users/jeanette/Dropbox/School/PhD/HIV_WA/HIVBackCalc/R/internal_fxns.R')

# Load libraries, data and debug file
# Note that when I used this function with the WA analysis, na="" (the default),
# and then I loaded a data formatting file. But here, the data are already
# formatted, so NAs are already "NA"
setup_hivbackcalc(workd='/Users/jeanette/Dropbox/School/PhD/HIV_WA/HIVBackCalc_App',
                  datafile='development/data_KC_sim.csv',
                  na="NA",
                  source_these=c('development/model.R', 
                                 'development/other.R'),
                  package_updated=FALSE, 
                  packagefile='development/debug_fxns.R')


#############################################################
# CURRENT MODEL RUN CODE FROM APP (9/10/15)
#############################################################

##### DEFINE DIAGNOSED COUNTS PER TIME UNIT (ASSUMED QTR, NOW)
time_min <- min(dataf$timeDx)
time_max <- max(dataf$timeDx)
allTimes <- seq(time_min, time_max, by=0.25)
obsCounts <- table(dataf$timeDx)
allCounts <- structure(rep(0,length(allTimes)),
                       class='table',
                       names=allTimes)
allCounts[names(allCounts)%in%names(obsCounts)] <- obsCounts


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

stats


