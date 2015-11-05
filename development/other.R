
#' Format data for use with the HIVBackCalc package
#'  
#' Creates a data frame of class 'testinghistories' with formatted and
#' calculated variables. Additionally returns a record of formatting
#' assumptions made for inconsistent entries
#'  
#' @param rawdata Unformatted data frame or file path to CSV file
#' @param eHARS Logical indicating whether the data are raw data 
#'        exported from eHARS in person view
#' @param assumptionNo Choice of assumption for those reporting never 
#'          having had a negative test. Default is 'age16', which imputes
#'          time from infection to diagnosis as min(age-16, 18 yrs). 'age16mid'
#'          instead uses the midpoint between age and 16, i.e. 
#'          min((age-16)/2, 18 yrs). 
#' @return Data frame of class "testinghistories" 
format_data <- function(rawdata, 
                        eHARS=FALSE,
                        assumptionNo='age16'
                        ) {
  
    require(plyr)

    # Read in data
    if (!is.data.frame(rawdata)) {
        rawdata  <- read.csv(rawdata,
                             na.strings='',
                             stringsAsFactors=FALSE)
    }

    # Helper function to flag records for cleaning
    recordFlag  <- function(df, logical, message) {
        # Set NA's in logical to FALSE
        logical[is.na(logical)] <- FALSE
        # Create flag variable if it doesn't exist
        if (!'flag'%in%colnames(df)) df$flag=''
        # Record flag and tidy up
        df$flag[logical] <- paste0(df$flag[logical], '; ', message) 
        df$flag <- gsub('^;', '', df$flag)
        # Add to assumptions table
        assumptionsN[assumptionsN$Issue==message,'N'] <<- sum(logical) 
        return(df)
    }

    # Construct empty assumptions table
    assumptionsN <- data.frame(Issue=c('Missing month',
                                    'Missing day',
                                    'Missing month special case',
                                    'Missing day special case',
                                    'Illogical last negative',
                                    'everHadNegTest not TRUE, FALSE or NA',
                                    'everHadNegTest inconsistent with infPeriod',
                                    'infPeriod=0',
                                    'infPeriod > aidsUB',
                                    'Age <=16 and no infPeriod',
                                    'Missing year',
                                    'Missing age'
                                    ),
                               Assumption=c('Month (diagnosis or last neg test) assumed July for computing infPeriod; diagnosis quarter randomly imputed',
                                            'Day (diagnosis or last neg test) assumed 15th for computing infPeriod',
                                            'Diagnosis and last neg test in same year and diagnosis before July; last neg assumed Jan 1',
                                            'Diagnosis and last neg test in same month and diagnosis before 15th; last neg day assumed 1st of month',
                                            'Last negative date overwritten as missing because recorded as at or after diagnosis',
                                            'everHadNegTest altered to NA',
                                            'everHadNegTest flag altered to match presence/absence of last neg test date',
                                            'infPeriod=0 changed to NA',
                                            'infPeriod capped at 18 years',
                                            'Removed from dataset because <=16 yrs and no recorded infPeriod',
                                            'Removed from dataset because no dx year',
                                            'Removed from dataset because no age'
                                            ),
                               N=NA)

    #############################################################
    # OVERVIEW: N, expected variables and labels for factors
    #############################################################
    # Also create a person ID
    origNames <- colnames(rawdata) 
    rawdata$personid <- 1:nrow(rawdata)
    rawdata <- rawdata[,c('personid', origNames)]

    # Keep formatted data separate from raw data
    dataf <- rawdata

    if (eHARS) {
        colnames(dataf) <- tolower(colnames(dataf))
        variables_expected <- c('rsh_state_cd',
                              'aids_dx_dt',
                              'cd4_first_hiv_dt',
                              'cd4_first_hiv_type',
                              'cd4_first_hiv_value',
                              'hiv_aids_age_yrs',
                              'hiv_dx_dt',
                              'race',
                              'screen_last_neg_dt',
                              'trans_categ',
                              'vl_first_det_dt',
                              'vl_first_det_value',
                              'birth_sex',
#                              'stateno',
                              'tth_last_neg_dt',
                              'tth_first_pos_dt',
                              'tth_ever_neg')
    } else {
        variables_expected <- c('hdx_age', 'infPeriod', 'everHadNegTest')
    } #end if (eHARS)

    not_in_dataf <- variables_expected[!variables_expected%in%colnames(dataf)]
    if (length(not_in_dataf)!=0) stop('Some variables are missing: \n', 
                                      paste(not_in_dataf,
                                            collapse='\n'))

    # If this is a re-format of a prior formatting job, switch back to the 
    # original variables. I don't think this will become an issue, but just 
    # in case 
    if (!eHARS) {
        if ('infPeriod_orig'%in%colnames(dataf)) {
            dataf$infPeriod <- dataf$infPeriod_orig
        }
        if ('everHadNegTest_orig'%in%colnames(dataf)) {
            dataf$everHadNegTest <- dataf$everHadNegTest_orig
        }
    }
    
    if (eHARS) {
        # Factors and labels
        races <- c('Hispanic',
                    'American Indian/Alaska Native',
                    'Asian',
                    'Black',
                    'Native Hawaiian/Pacific Islander',
                    'White',
                    'Legacy Asian/Pacific Islander',
                    'Multi-race',
                    'Unknown')
        modes <- c('Adult MSM', 
                    'Adult IDU',
                    'Adult MSM & IDU',
                    'Adult received clotting factor',
                    'Adult heterosexual contact',
                    'Adult received transfusion/transplant',
                    'Perinatal exposure, HIV diagnosed at age 13 years or older',
                    'Adult with other confirmed risk',
                    'Adult with no identified risk (NIR)',
                    'Adult with no reported risk (NRR)',
                    'Child received clotting factor',
                    'Perinatal exposure',
                    'Child received transfusion/transplant',
                    'Child with other confirmed risk',
                    'Child with no identified risk (NIR)',
                    'Child with no reported risk (NRR)',
                    'Risk factors selected with no age at diagnosis')
        dataf <- transform(dataf,
                         new_race=as.character(factor(race, levels=1:9,
                                         labels=races)),
                         new_mode=as.character(factor(trans_categ, 
                                                      levels=c(1:13,18:20,99), 
                                                      labels=modes)),
                           stringsAsFactors=FALSE)
        # Some renaming
        dataf <- rename(dataf,c('hiv_aids_age_yrs'='hdx_age',
                                  'birth_sex'='sex'))
        
    } # end if (eHARS)

    #############################################################
    # SUMMARIZE: Summarize or tabulate variables and store in
    #            a table
    #############################################################

    # Numeric variables
    summaries <- lapply(dataf, FUN=summary)
    numeric <- sapply(summaries, FUN=function(x) "Min."%in%names(x))
    summaries1 <- summaries[numeric]
    summaries1 <- lapply(summaries1, function(x){
                        if (!"NA's"%in%names(x)) x <- c(x,0)
                        else x
                             })
    table1 <- do.call('rbind', summaries1)
    colnames(table1)[7] <- "NA's"


    # Non-numeric
    summaries2 <- summaries[!numeric]
    values <- sapply(names(summaries2), function(x) { 
                     u <- unique(dataf[,x])
                     if (length(u)>25) u <- c(u[1:25], '...TRUNCATED')
                     paste(u,collapse=',') 
              })
    nas <- sapply(names(summaries2), function(x) {
                  sum(is.na(dataf[,x]) | dataf[,x]=='')
              })
    table2 <- data.frame(Variable=names(values), 
                         NAs=nas,
                        Values=values,
                        stringsAsFactors=FALSE)


    #############################################################
    # COLLAPSE RACE AND MODE OF DIAGNOSIS
    #############################################################

    if (eHARS) {
        collapsed_race <- c('White', 'Black', 'Hispanic', 'Asian', 
                            'Native', 'Multi/Other')
        collapsed_mode <- c('MSM', 'Hetero', 'Blood/Needle/Other')
        dataf <- within(dataf, {
            race6 <- as.character(new_race)
            race6[race6 %in% c("American Indian/Alaska Native", 
                               "Native Hawaiian/Pacific Islander")] <- 'Native'
            race6[race6 %in% "Legacy Asian/Pacific Islander"] <- 'Asian'
            race6[race6 %in% c("Multi-race","Unknown")] <- 'Multi/Other'
            mode3 <- as.character(new_mode)
            mode3[mode3 %in% c('Adult MSM','Adult MSM & IDU')] <- 'MSM'
            mode3[mode3 %in% c('Adult heterosexual contact',
                               'Adult with no identified risk (NIR)')] <- 'Hetero'
            mode3[!mode3 %in% c('MSM', 'Hetero')] <- 'Blood/Needle/Other'
    #        race6 <- factor(race6,
    #                       labels=collapsed_race,
    #                       levels=collapsed_race)
    #        mode3 <- factor(mode3,
    #                       levels=collapsed_mode,
    #                       labels=collapsed_mode)
    #        mode2 <- factor(ifelse(mode3 %in% 'MSM', 'MSM', 'non-MSM'))
             mode2 <- ifelse(mode3 %in% 'MSM', 'MSM', 'non-MSM')
            # FOR NOW: make the main mode=mode2
            mode <- mode2
        })
    } # end if (eHARS)

    #############################################################
    # DEFINE AGE GROUPS
    #############################################################
    dataf <- transform(dataf,
                     agecat5=cut(hdx_age,
                                 breaks=c(0,seq(20,70,by=5),100),
                                 include.lowest=TRUE,
                                 right=TRUE,
                                 labels=c('<=20',
                                          '21-25',
                                          '26-30',
                                          '31-35',
                                          '36-40',
                                          '41-45',
                                          '46-50',
                                          '51-55',
                                          '56-60',
                                          '61-65',
                                          '66-70',
                                          '71+')))

    # Record missing age
    dataf <- recordFlag(dataf, is.na(dataf$hdx_age),
                        'Missing age')

    #############################################################
    # FORMAT DATES AND CREATE INFPERIOD
    #############################################################

    if (eHARS) {
        # Helper function to work with dates
        # For each date, need a fake date if month and/or day are missing 
        # plus an imputed quarter if month is missing
        # If LNT (last negative test) dates, there will be additional
        # checks against the HIV dx date to ensure that imputed
        # values don't create illogical LNTs

        get_dates <- function(timevar, LNT=FALSE) {

            year <- suppressWarnings(as.numeric(substr(timevar,1,4)))
            month <- substr(timevar,5,6)
            day <- substr(timevar,7,8)
            missing_month <- !is.na(year) & month=='..'
            missing_day <- !is.na(year) & day=='..' & !month=='..'
            # Create a year-quarter variable, imputing a quarter if necessary
            set.seed(98103)
            browser()
            yrqtr <- year + 
                suppressWarnings(ifelse(missing_month, sample(c(0,0.25,0.5,0.75)), 
                                        ceiling(as.numeric(month)/3)*0.25-0.25))
            # Create an  _imputed date for calculating inter-test intervals
            # 15th of the month if only month is known; July 1 if only year known
            day <- ifelse(missing_month, '01', ifelse(missing_day, '15', day))
            month <- ifelse(missing_month, '07', month)

            if (LNT) {
                # Edit assumptions for those whom it causes a problem
                # First, LNT in same month as dx and dx date before 15th:
                # use 1st of the month not 15th
                missing_day_1st <- !is.na(dataf$dxTmp) & 
                                    format(dataf$dxTmp, '%y')==substr(timevar,3,4) & 
                                    format(dataf$dxTmp, '%m')==month & 
                                    missing_day & 
                                    as.numeric(format(dataf$dxTmp, '%d'))<=15
                day[missing_day_1st] <- '01'
                # LNT in same year as dx and dx date before July 1: 
                # use Jan 1
                missing_month_Jan <- !is.na(dataf$dxTmp) & 
                                     format(dataf$dxTmp, '%y')==substr(timevar,3,4) & 
                                     missing_month & 
                                     as.numeric(format(dataf$dxTmp, '%m'))<=7
                month[missing_month_Jan] <- '01'
                day[missing_month_Jan] <- '01'
            } else {
                missing_month_Jan <- NULL
                missing_day_1st <- NULL
            }

            dateChar <- apply(cbind(year,month,day),1,paste,collapse='-')
            dateChar[dateChar=='NA-NA-NA'] <- ''
            dateImp <- as.Date(dateChar,"%Y-%m-%d")

            return(list(dateImp=dateImp, 
                        year=year,
                        yrqtr=yrqtr,
                        missMonth=missing_month, 
                        missDay=missing_day,
                        missJan=missing_month_Jan,
                        miss1st=missing_day_1st))
        }

        # Diagnosis date (temporarily attach to dataf)
        dxDate <- get_dates(dataf$hiv_dx_dt)
        dataf$dxTmp <- dxDate$dateImp
        # Last negative test date
        negDate <- get_dates(dataf$tth_last_neg_dt, LNT=TRUE)
        dataf <- transform(dataf,
                           dxTmp=NULL,
                           yearDx=dxDate$year,
                           timeDx=dxDate$yrqtr,
                           infPeriod=as.numeric(dxDate$dateImp-
                                                negDate$dateImp)/365,
                           stringsAsFactors=FALSE)

        # Record assumptions
        dataf <- recordFlag(dataf, dxDate$missMonth | negDate$missMonth,
                            'Missing month')
        dataf <- recordFlag(dataf, dxDate$missDay | negDate$missDay,
                            'Missing day')
        dataf <- recordFlag(dataf, negDate$missJan,
                            'Missing month special case')
        dataf <- recordFlag(dataf, negDate$miss1st,
                            'Missing day special case')

        # Missing HIV dx year
        dataf <- recordFlag(dataf, is.na(dxDate$year),
                            'Missing year')

        # Illogical last negative
        dataf <- recordFlag(dataf, dataf$infPeriod<=0,
                            'Illogical last negative')
        dataf <- within(dataf, {
                        infPeriod[infPeriod<=0] <- NA
                           })


        #############################################################
        # CREATE everHadNegTest after creating infPeriod
        #############################################################
        # Define everHadNegTest based on tth_ever_neg
        dataf <- transform(dataf, 
                         everHadNegTest=ifelse(tth_ever_neg=='Y', TRUE, 
                                               ifelse(tth_ever_neg=='N', FALSE, NA)))
        #with(dataf,table(everHadNegTest, tth_ever_neg, useNA='always'))

        # Look at actual infPeriod values by everHadNegTest
        #ddply(dataf, .(everHadNegTest), function(x) c(summary(x$infPeriod)))

    } else { # end if (eHARS)

        # Save the old version
        dataf$infPeriod_orig <- dataf$infPeriod

        # If not creating infPeriod from eHARS dates, check that it is
        # numeric as expected
        infMode <- mode(dataf$infPeriod_orig)
        if (infMode!='numeric') {
            # Attempt a numeric conversation
            dataf$infPeriod <- as.numeric(dataf$infPeriod_orig)
        }
    }

    #############################################################
    # RECONCILE everHadNegTest and infPeriod
    #############################################################

    if (!eHARS) {
        # Store original if it exists
        dataf$everHadNegTest_orig <- dataf$everHadNegTest
    }

    ## ---- fix_everHadNegTest ---- 
    checkEverHad <- NULL
    if (!is.logical(dataf$everHadNegTest)) {
        if (is.character(dataf$everHadNegTest)) {
            # Convert any actual NAs to string NAs, for simplicity's sake
            dataf$everHadNegTest[is.na(dataf$everHadNegTest)] <- 'NA'
            wrongEverHad <- !dataf$everHadNegTest%in%c('TRUE', 'FALSE', 'NA')
            tmpEverHad <- ifelse(wrongEverHad | dataf$everHadNegTest=='NA',
                                 NA,
                                 ifelse(dataf$everHadNegTest=='TRUE',
                                        TRUE, FALSE))
            checkEverHad <- table(tmpEverHad, dataf$everHadNegTest, useNA='ifany')
            dataf$everHadNegTest <- tmpEverHad
        } else stop("In format_data, everHadNegTest is not logical or character")
    } else wrongEverHad <- rep(FALSE, nrow(dataf))

    # Record assumptions and flag
    dataf <- recordFlag(dataf, wrongEverHad, 
                        'everHadNegTest not TRUE, FALSE or NA')

    # Only do this section if the data have not already been formatted with the
    # assumption for No's
    nos <- subset(dataf, everHadNegTest==FALSE)
    already_formatted <- ifelse((sum(is.na(nos$infPeriod))!=0),
                                FALSE, TRUE)

    if (!already_formatted) {
        ## ---- fix_everHadNegTest_toTRUE ----
        toTRUE1 <- !is.na(dataf$everHadNegTest) & 
                   !dataf$everHadNegTest & 
                   !is.na(dataf$infPeriod)
        toTRUE2 <- is.na(dataf$everHadNegTest) & !is.na(dataf$infPeriod)
        dataf$everHadNegTest[toTRUE1] <- TRUE
        dataf$everHadNegTest[toTRUE2] <- TRUE

        ## ---- fix_everHadNegTest_toFALSE ----
        toFALSE <- !is.na(dataf$everHadNegTest) & 
                   dataf$everHadNegTest & 
                   is.na(dataf$infPeriod)
        dataf$everHadNegTest[toFALSE] <- FALSE

        # Record assumptions and flag
        dataf <- recordFlag(dataf, toTRUE1 | toTRUE2 | toFALSE,
                         'everHadNegTest inconsistent with infPeriod')
    }
              
    ## ---- check_everHadNegTest ----
 #   checkEver <- with(dataf,table(everHadNegTest, 
 #                                TID_NA=is.na(infPeriod), useNA='always')))

    #############################################################
    # EDIT infPeriod
    #############################################################

    # Cap at AIDS upper bound of ~18 years
    aidsUB <- qweibull(.95,shape=2.516,scale=1/0.086) #17.98418
    dataf <- recordFlag(dataf, !is.na(dataf$infPeriod) & 
                               dataf$infPeriod>aidsUB,
                        'infPeriod > aidsUB')
    
    # Apply desired assumption for the No's
    fixNo <- function(dataf, assumptionNo) {
        switch(assumptionNo,
               age16 = {
                    dataf <- transform(dataf,
                                       infPeriod=ifelse(everHadNegTest, 
                                                        pmin(infPeriod, aidsUB), 
                                                        ifelse(!everHadNegTest, 
                                                               pmin(hdx_age-16, 
                                                                    aidsUB), 
                                                               NA)))
               },
               age16mid = {
                    dataf <- transform(dataf,
                                       infPeriod=ifelse(everHadNegTest, 
                                                        pmin(infPeriod, aidsUB), 
                                                        ifelse(!everHadNegTest, 
                                                               pmin((hdx_age-16)/2, 
                                                                    aidsUB), 
                                                               NA)))
               }
               )
        return(dataf)
    }
    dataf <- fixNo(dataf, assumptionNo)

    # Remove cases who are too young for the impute-infPeriod assumption
    dataf <- recordFlag(dataf, 
                        dataf$hdx_age<=16 & (!dataf$everHadNegTest | 
                                        is.na(dataf$everHadNegTest)),
                        message='Age <=16 and no infPeriod')
    dataf <- subset(dataf, !(hdx_age<=16 & (!everHadNegTest | 
                                            is.na(everHadNegTest))))
    # Set infPeriod=0 to NA
    zeroInf <- !is.na(dataf$infPeriod) & dataf$infPeriod==0
    dataf$infPeriod[zeroInf] <- NA
    dataf <- recordFlag(dataf, zeroInf,
                        message='infPeriod=0')


    #############################################################
    # CREATE infPeriod_imputeNA
    #############################################################
    dataf <- within(dataf,{ 
        infPeriod_imputeNA=ifelse(is.na(everHadNegTest),
                                  pmin(hdx_age-16, aidsUB),
                                  infPeriod)
    })

    #############################################################
    # Remove cases with missing year and/or age
    #############################################################
    dataf <- subset(dataf, !is.na(hdx_age) & !is.na(yearDx))

    class(dataf) <- append(class(dataf), 'testinghistories')
    return(list(data=dataf,
                rawdata=rawdata,
                assumptions=assumptionsN,
                checkEverHad=checkEverHad,
                rawVarSum=list(Numeric=table1,Categorical=table2)))
}

#############################################################
# apply_assumptions
#############################################################

apply_assumptions <- function(dataf, assumption) {

    if (!is.logical(dataf$everHadNegTest)) {
        stop('In apply_assumptions, everHadNegTest variable is not a logical')
    }

    aidsUB <- qweibull(.95,shape=2.516,scale=1/0.086) #17.98418

    switch(assumption,
           'age16' = {

                dataf <- transform(dataf, 
                                   assumption= ifelse(everHadNegTest, 
                                                   pmin(infPeriod, aidsUB), 
                                                   ifelse(!everHadNegTest, 
                                                          pmin(hdx_age-16, 
                                                               aidsUB), 
                                                          NA)))
           },
           'age16mid' = {

                dataf <- transform(dataf, 
                                   assumption= ifelse(everHadNegTest, 
                                                   pmin(infPeriod, aidsUB), 
                                                   ifelse(!everHadNegTest, 
                                                          pmin((hdx_age-16)/2, 
                                                               aidsUB), 
                                                          NA)))
           }

    )
    return(dataf$assumption)
}
    
#############################################################
# check_data
#############################################################

check_data <- function(dataf) {

    toReturn <- vector('list', length=5)
    names(toReturn) <- c('everHadNegTest levels',
                         'Years without testing histories',
                         'Years with sparse diagnoses',
                         'Assumption for everHadNegTest=FALSE',
                         'Maximum infPeriod')

    # Levels of everHadNegTest
    checkNoTH <- with(dataf, table(yearDx, everHadNegTest, useNA='ifany'))

    wrongLevels <- 
        colnames(checkNoTH)[!colnames(checkNoTH)%in%c('TRUE', 'FALSE', NA)]

    toReturn[['everHadNegTest levels']] <- 
        ifelse(length(wrongLevels)==0, 
               'everHadNegTest is coded correctly',
               paste(wrongLevels, 'is not an acceptable value for everHadNegTest'))

    # Are there years of data with no testing histories?
    noTH <- checkNoTH[checkNoTH[,'TRUE']==0,]
    toReturn[['Years without testing histories']]  <- 
        ifelse(nrow(noTH)==0, 
               paste0('All years (',
                      rownames(checkNoTH)[1],
                      '-',
                      rownames(checkNoTH)[nrow(checkNoTH)],
                      ') have at least one record with a date of last negative test'),
               paste('The following years have no records with a date of last negative test:', paste(rownames(noTH), collapse=',')))

    # Are there years of data with <5 diagnoses?
    DxByYear <- rowSums(checkNoTH)
    LowDx <- DxByYear[DxByYear<=10]
    toReturn[['Years with sparse diagnoses']]  <- 
        ifelse(length(LowDx)==0, 
               paste0('All years (',
                      rownames(checkNoTH)[1],
                      '-',
                      rownames(checkNoTH)[nrow(checkNoTH)],
                      ') have >=10 diagnoses'),
               paste('The following years have <=10 diagnoses:', paste(names(LowDx), collapse=',')))

    # If necessary, convert everHadNegTest in order to check the No assumption
    if (length(wrongLevels)>0) {
        if (is.character(dataf$everHadNegTest)) {
            wrongEverHad <- !dataf$everHadNegTest%in%c('TRUE', 'FALSE', 'NA')
            tmpEverHad <- ifelse(wrongEverHad | dataf$everHadNegTest=='NA',
                                 NA,
                                 ifelse(dataf$everHadNegTest=='TRUE',
                                        TRUE, FALSE))
            dataf$everHadNegTest <- tmpEverHad
        } else stop("In check_data, everHadNegTest is not logical or character")
    }
    
    # Assumptions
    aidsUB <- qweibull(.95,shape=2.516,scale=1/0.086) #17.98418

    # No's: Age 16
    everHadNo <- !is.na(dataf$everHadNegTest) & !dataf$everHadNegTest
    checkAge16  <- 
        ifelse(max(abs(dataf$infPeriod[everHadNo]-
                   apply_assumptions(dataf, 'age16')[everHadNo]))>0.02,
               'Age 16 assumption NOT applied',
               'Age 16 assumption applied')
    checkAge16mid  <- 
        ifelse(max(abs(dataf$infPeriod[everHadNo]-
                   apply_assumptions(dataf, 'age16mid')[everHadNo]))>0.02,
               'Age 16 midpoint assumption NOT applied',
               'Age 16 midpoint assumption applied')

    toReturn[['Assumption for everHadNegTest=FALSE']] <- 
        c(checkAge16, checkAge16mid)

    toReturn[['Maximum infPeriod']] <- 
        ifelse(max(dataf$infPeriod, na.rm=TRUE)>18,
               'Maximum infPeriod is > 18',
               'Maximum infPeriod is <= 18')

    return(toReturn)
}



