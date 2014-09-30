############################################################
# This file makes a formatted KC (MSM) testing histories file 
# just for development purposes. Ultimately, we'll have to 
# decide how to optimally divide the formatting between user 
# prep and in-app work
#
# The code is taken from analysis_WA/combine_MSM_report.Rnw
############################################################


#############################################################
# SETUP
#############################################################
#rm(list=ls())
# TEMPORARY: SOURCE FUNCTIONS
source('/Users/jeanette/Dropbox/School/PhD/HIV_WA/HIVBackCalc/R/other.R')

# ALL-WA
# Load libraries, data and data-cleaning file
# Eventually this function should return the cleaned dataset,
# but data-cleaning has the name hardcoded as msm and I'm not
# going to generalize that right now
setup_hivbackcalc(workd='/Users/jeanette/Dropbox/School/PhD/HIV_WA',
                  datafile='data/WA_BACKCALC_DATA_v2.csv',
                  source_these='analysis_WA/format_data.R')

# MSM-KC
setup_hivbackcalc(workd='/Users/jeanette/Dropbox/School/PhD/HIV_WA',
                  datafile='data/MSM_KingCounty_rev.csv',
                  source_these='analysis_KC/data-cleaning_JKB.R',
                  loadlib=FALSE,
                  msm=TRUE)


#############################################################
# FORMAT
#############################################################
# Generate and rename cols so we can combine
msm <- rename(msm, c('everTested'='everHadNegTest',
                     'hiv_age_yrs'='hdx_age'))
msm$mode <- 'MSM'
msm <- transform(msm, agecat5=cut(hdx_age,
                              breaks=c(0,seq(20,70,by=5),85),
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
                                       '71-85')))

race_levels <- c('White', 'Black', 'Hisp', 'Asian', 'Native', 'Multi')
msm <- within(msm, {
                race <- as.character(racel)
                race[race=='1White'] <- 'White'
                race[race=='2Black'] <- 'Black'
                race[race=='3Hisp'] <- 'Hisp'
                race[race=='4Asian'] <- 'Asian'
                race[race=='4PI' | race == '5AmInd'] <- 'Native'
                race[race=='6Multi'] <- 'Multi'
                race <- factor(race,
                                labels=race_levels,
                                levels=race_levels)
                })
# Combine
these_cols <- c('hdx_age', 'agecat5', 'mode', 'race', 
                'everHadNegTest', 'timeDx', 'yearDx',
                'infPeriod', 'infPeriod_imputeNA')
all <- rbind(data.frame(Population='KC',
                        msm[,these_cols]),
             data.frame(Population='WA',
                        dataf[,these_cols]))

#############################################################
# SAVE FOR DEVELOPMENT
#############################################################

write.csv(all,
          file='/Users/jeanette/Dropbox/School/PhD/HIV_WA/HIVBackCalc_App/development/data_KC+WA_formatted.csv',
          row.names=FALSE)


