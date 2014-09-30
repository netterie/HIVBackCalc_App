
######################################################################
# THIS NEEDS TO BE MOVED TO THE model.R FILE
######################################################################

# Alternative to estimateProbDist, the base case function,
# that uses the upper bound assumption
empirProbDist <- function(infPeriod,intLength=1){
  ti <- sort(infPeriod[!is.na(infPeriod) & infPeriod > 0])
  n <- length(ti)
  qi <- function(u) {
    uind <- sum(ti<=u)/n
    if (is.na(uind)) 
      return(0)
    uind
  }
  pidCalc <- function(i) {
    sapply(i, function(ii) {
      qi((ii + 1) * intLength) - qi(ii * intLength)
    })
  }
  m <- max(ti/intLength) + 1
  pidProbs <- pidCalc(0:m)
  pid <- function(i) {
    ifelse(i > m, 0, pidProbs[i + 1])
  }
  pid
}

######################################################################
# FIGURE 1: CUMULATIVE TIME FROM LAST NEG TO INFECTION (1-EMPIRICAL CDF) 
######################################################################

fig1 <- function(infPeriod, returnFig=TRUE, d1=NULL, legendpos=NULL) {
    if (is.null(d1)) {
        # Sort infPeriod times - exclude zeroes, who never had a negative test
        ti <- sort(infPeriod[!is.na(infPeriod) & infPeriod > 0])
        nUB <- length(ti) #1217
        # Function to compute 1-cdf
        # This reflects the UB assumption because it assumes that
        # time of infection=time of last negative test
        qUB <- function(u) {
          uind <- sum(ti<=u)/nUB
          if (is.na(uind)) 
            return(0)
          uind
        }
        # Function to compute at each infPeriod,
        # the ratio of the # of diagnoses occurring at/after the time
        # to total observed diagnoses...so, # implied diagnoses per 1 time unit
        # What was the intention for eta?
        pi <- function(i, eta, ti = ti) {
          sapply(i, function(ii) {
            # Get those infPeriod times that are >= ii, a unique infPeriod time
            ints <- ti[ti >= ii]
            # Average # diagnoses/unit time for times >= ii
            # Is this what assumes infection is uniformly distributed in an infPeriod?
            sum(1/ints)/length(ti)
          })
        }
        # Unique infPeriod times 
        uti <- unique(ti) #652, of #1217 possible
        # Multiply implied diagnoses/unit time by actual time step (vs 1 unit time)
        # diff(c(0,uti)) - for each of 0 and the unique infPeriods, this generates a 1st order lag
        p <- pi(uti, , ti) * diff(c(0, uti))
        # Cumulative distribution of p
        cs <- cumsum(p)
        # Function to evaluate cs at selected times (u), 
        # returning 0 for presumably only u=0 
        qi <- function(u) { 
          # Indicators for the 1st time in uti (unique infPeriod times) >= u
          uind <- rev(which(uti <= u))[1]
          if (is.na(uind)) 
            return(0)
          # Return the cumulative distribution of p for that indicator
          cs[uind]
        }
        s <- seq(from=0,to=20,length.out=500)
        # Base case 1-cdf
        est <- sapply(s,qi)
        # Upper bound 1-cdf
        ub <- sapply(s,qUB)

# Plot
        d1 <- rbind(data.frame(var="Base Case  ",value=est,Time=s),
                    data.frame(var="Upper Bound  ",value=ub,Time=s))

    }

    if (is.null(legendpos)) legendpos='bottom'

    p <- ggplot(d1) + geom_line(aes(x=Time,y=1-value,color=var)) + 
      scale_color_hue(name="") +
      theme_bw() + 
      ylab("Undiagnosed Fraction") +
      xlab("Time Since Infection") +
      scale_x_continuous(expand=c(0,.2))
      theme(legend.position=legendpos)

    if (returnFig) return(p) else return(d1)
}

######################################################################
# FIGURE 1: combine fig 1 with and without imputation
######################################################################

fig1combined <- function(df, 
                         panel=NULL,
                         newcases=c('Base Case', 
                                    'Worst Case (Obs)', 
                                    'Worst Case (Miss)'),
                         legendposition=NULL
                         ) {

    fig1combined_internal <- function(df, newcases, returnFig=TRUE, lp=legendposition) {
        fig1d1 <- fig1(df$infPeriod, returnFig=FALSE)
        fig1d2 <- fig1(df$infPeriod_imputeNA, returnFig=FALSE)
        fig1d1 <- transform(fig1d1, var=gsub('[[:space:]]+$', '', as.character(var)))
        fig1d2 <- transform(fig1d2, var=gsub('[[:space:]]+$', '', as.character(var)))
        fig1d1[fig1d1$var=='Upper Bound','var'] <- newcases[2]
        fig1d2[fig1d2$var=='Upper Bound','var'] <- newcases[3]
        newd1 <- rbind(fig1d1, subset(fig1d2, var==newcases[3]))
        newd1 <- transform(newd1,
                           var=factor(var, levels=newcases, labels=newcases))
        newfig1 <- fig1(df$infPeriod, returnFig=TRUE, d1=newd1, 
                        legendpos=lp)
        if (returnFig) return(newfig1) else return(newd1)
    }

    if (is.null(panel)) return(fig1combined_internal(df, newcases))
    else {
        figs <- NULL
        figsfull <- NULL
        for (u in unique(df[,panel])) {
            figs[[u]] <- fig1combined_internal(df[df[,panel]==u,],
                                               newcases,
                                               returnFig=FALSE)
            figs[[u]] <- transform(figs[[u]],
                                   group=u)
            figsfull <- rbind(figsfull, figs[[u]])
        }
        newfig2 <- fig1(df$infPeriod, returnFig=TRUE, d1=figsfull, legendpos=legendposition)
        newfig2 <- newfig2 + facet_grid(.~group)
        return(newfig2)
    }

}

######################################################################
# SETUP
######################################################################

#' Setup function
#' @param workd Working directory full file path
#' @param datafile Path to data, either full or from the workd
#' @param source_these Vector of paths to other files to source, either full or from the workd
#' @param loadlib Logical indicating whether to load libries
#' @param msm Logical indicating whether this is the KC MSM analysis where the data frame was called msm
setup_hivbackcalc = function(workd, datafile, source_these, loadlib=TRUE,
                             msm=FALSE) {
  
    if (loadlib) {
      cat('Loading libraries...\n')
      # Load libraries
      # Eventually, make these dependencies of the HIBBackCalc package?
      library(HIVBackCalc)
      library(reshape2)
      library(plyr)
      library(ggplot2)
      library(scales)
      library(Hmisc)
    }

  # Working directory
  workd <<- workd
  
  # Load data
  cat('Loading data and storing it in object msm...\n')
  if (msm) {
      msm <<- read.csv(file.path(workd,datafile),na.string="",stringsAsFactor=FALSE) 
  } else {
      dataf <<- read.csv(file.path(workd,datafile),na.string="",stringsAsFactor=FALSE) 
  }

  # Source files
  for (f in source_these) {
    cat('Sourcing', f, '...\n')
    source(file.path(workd,f))
  }
  
}


######################################################################
# TABULATE everHadNegTest in percents
######################################################################
tabulate_everHadNegTest <- function(df, variables, supercolumn=FALSE) {

    vars <- list(NULL)
    for (v in 1:length(variables)) {

      tab <- ddply(df, variables[[v]], function(x, TN=nrow(df)) {
              n <- nrow(x)
              c(N=n,
                `Column Percent`=round(100*n/TN,0),
                `Percent Yes`=round(100*sum(x$everHadNegTest, na.rm=TRUE)/n,0),
                `Percent No`=round(100*sum(!x$everHadNegTest, na.rm=TRUE)/n,0),
                `Percent Missing`=round(100*sum(is.na(x$everHadNegTest))/n,0))
             })
      if (supercolumn) {
          colnames(tab)[1] <- 'Subgroup'
          tab <- data.frame(Characteristic=rep('',nrow(tab)),
                            tab,
                            stringsAsFactors=FALSE)
          tab$Characteristic[1] <- names(variables)[v]
      }

      vars[[v]] <- tab
    }

    vars <- do.call(rbind, vars)
    return(vars)
}

######################################################################
# PLOT everHadNegTest over time in percents
######################################################################
plot_everHadNegTest <- function(df, panel=NULL) {
    keep.vars <- c(panel, 'yearDx', grep('Percent ', colnames(df),
                                         value=TRUE))
    these.ids <- c(panel, 'yearDx')
    df <- df[,keep.vars]
    colnames(df) <- gsub('Percent ','',colnames(df))
    df <- melt(df, id.vars=these.ids)
    if (!is.null(panel)) df$Group <- df[,panel]

    p <- ggplot(df,aes(x=yearDx,y=value,group=variable))  +   
      geom_line(aes(color=variable)) +
      geom_point(aes(color=variable)) + 
      theme_bw()+
      theme(legend.position='bottom',axis.text.x=element_text(angle=90)) + 
      scale_color_hue(name="Ever had negative test?") + 
      scale_x_continuous(breaks=seq(min(df$yearDx),max(df$yearDx),by=2))+
      xlab("Time") + ylab("Percent") 

   if (!is.null(panel)) p <- p + facet_grid(.~Group)
   return(p)
}

#############################################################
# PLOT DIAGNOSIS COUNTS PER QUARTER
#############################################################
plot_qtrDx  <- function(df, panel=NULL) {

    if (is.null(names(panel))) names(panel) <- panel
    variables <- c('timeDx', panel[1])

    qtrCounts <- ddply(df, variables, function(x) nrow(x))

    if (!is.null(panel)) {
        qtrCounts$group <- qtrCounts[,names(panel)[1]] 
        legendpos <- 'bottom'
    } else {
        qtrCounts$group <- 'All'
        legendpos <- 'none'
    }

    if (!is.null(panel)) {
        if (length(panel)==2) {
            variables2 <- c('timeDx', panel[2])
            qtrCounts2 <- ddply(df, variables2, function(x) nrow(x))
            qtrCounts2$group <- qtrCounts2[,names(panel)[2]]
            qtrCounts$biggroup <- names(panel)[1]
            qtrCounts2$biggroup <- names(panel)[2]
            qtrCounts <- rbind(qtrCounts2[,c('timeDx','V1','group', 'biggroup')], 
                               qtrCounts[,c('timeDx','V1','group', 'biggroup')])
        }
    }

    p <- ggplot(qtrCounts,aes(x=timeDx,y=V1,group=group))  +   
      geom_line(aes(color=group)) +
      geom_point(aes(color=group)) + 
      theme_bw()+
      scale_color_hue(name="") +
      theme(legend.position=legendpos,axis.text.x=element_text(angle=90)) + 
      scale_x_continuous(breaks=seq(min(df$yearDx),max(df$yearDx),by=1))+
      xlab("Time") + ylab("Diagnoses") 
    
    if (!is.null(panel)) {
        if (length(panel)==2) p <- p +facet_grid(.~biggroup) 
    }

    return(p)
}

######################################################################
# SUMMARIZE TID
######################################################################

#' Summarize infection period data
#' @param infPeriod Vector of times from last negative test to diagnosis
#' @param bygroup Optional vector defining subgroups
summarize_infPeriod = function(
  infPeriod, 
  bygroup=NULL
) {

  # Group indicator
  if (is.null(bygroup)) group = rep('All', length(infPeriod)) else group=bygroup
  if (sum(is.na(group))!=0) {
      group=as.character(group)
      group[is.na(group)]<-'NA'
  }
  # Create data frame
  df <- data.frame(infPeriod=infPeriod, Group=group) 

  # Summarize
  sumstats <- ddply(df, .(Group), function(x) c(summary(x$infPeriod)[-7], IsNA=sum(is.na(x$infPeriod)),N=nrow(x)))
  sumstats[,"Percent Missing"] = round(100*sumstats[,"IsNA"]/sumstats[,"N"],2)
  
  # Return object
  r = list(stats=sumstats)
  
  # Oneway test for mean infPeriod across groups
  if (!is.null(bygroup)) {
    r$oneway = oneway.test(infPeriod~Group,data=df)
  }
  
  return(r)
  
}

######################################################################
# ESTIMATE TOTAL AND UNDIAGNOSED INCIDENCE (INFECTION) COUNTS
######################################################################

#' Estimate incidence: all and undiagnosed
#' @param TID Vector containing TID, in years
#' @param impute Logical indicating whether to impute missing TID data (TRUE) or allow it to be excluded (FALSE)
#' @param age Vector containing ages at diagnosis, in years
#' @param diagnosedCounts Vector containing counts of diagnosed cases per interval
#' @param upperBound Logical indicating whether to use upper bound assumption. FALSE=use base case assumption
#' @param intervalLength Interval length in years
#' @param printProgress Logical indicating whether to print out progress during the backcalculation
#' @param runBoth Logical indicating whether to run the next case (base case or upper bound) as well
#' @param prevResults store previous results of runBackCalc here and indicate recursive use to run the next case (base case or upper bound)
runBackCalc = function(TID, 
                       impute,
                       age,
                       diagnosedCounts, 
                       upperBound,
                       intervalLength=0.25, 
                       printProgress=FALSE,
                       runBoth=FALSE,
                       prevResults=NULL) {

    cases = c('base case', 'upper bound')
    estType = cases[sum(upperBound)+1]

    call <-  paste('Call:\n   ', estType, 'backcalculation using data from', 
        length(diagnosedCounts), 'intervals of length', 
        intervalLength, 'years\n')
    cat(call)

    # Impute missing TID?
    TID_imputed <- TID
    if (impute) {
        cat('   \nImputing for TID=NA...')
        TIDNA <- is.na(TID)
        TID_imputed[TIDNA] <- pmin(age[TIDNA]-16,qweibull(.95,shape=2.516,scale=1/0.086))
    }

    # Define the discrete time probability distribution function of 
    # time from infection to diagnosis, using the base case 
    # assumption that infection is uniformly distributed between
    # time of last negative test and time of diagnosis
    if (upperBound) {
        pid <- empirProbDist(infPeriod=TID_imputed,
                             intLength=intervalLength)
    } else pid <- estimateProbDist(infPeriod=TID_imputed,
                                   intLength=intervalLength)

    # Set y = nPrevInt NA's + number of diagnoses per quarter-year to indicate
    # that we want to backcalculate incidence for 100 time steps prior to 
    # our data
    nPrevInt <- 100
    y <- c(rep(NA,nPrevInt),diagnosedCounts)

    # Backcalculate quarterly incidence for all
    allInc <- estimateIncidence(y,pid,
                             gamma=.1,verbose=printProgress,
                             tol=10^-4)

    # Estimate undiagnosed incidence given incidence, diagnosis, 
    # and pid function
    undiag <- estimateUndiagnosed(allInc)

    results <-  list(list(call=call, 
                          percMissTID=100*sum(is.na(TID)/length(TID)),
                          allInc=allInc$lambda[(nPrevInt+1):length(y)], 
                          undiag=undiag[(nPrevInt+1):length(y)]),
                     prevResults[[1]])
    names(results) <- c(estType, cases[!cases%in%estType])

    # Return or run next case
    if (runBoth) {
        runBackCalc(TID, 
                    impute,
                    age,
                    diagnosedCounts, 
                    !upperBound, 
                    intervalLength, 
                    printProgress, 
                    runBoth=FALSE, 
                    prevResults=results)
    } else return(results)

}


######################################################################
# SUMMARIZE runBackCalc RESULTS
######################################################################

#' Estimate incidence: all and undiagnosed
#' @param results List of length 2, where each list has elements allInc (incidence counts per interval) and undiag (undiagnosed counts per interval), and the lists are named 'base case' and 'upper bound'
#' @param diagnosedCounts Vector containing counts of diagnosed cases per interval
#' @param times Vector of times that describe the intervals
summarize_runBackCalc = function(results,
                                 diagnosedCounts,
                                 times) {

    # All Incidence
    d <- rbind(
      data.frame(time=times,var="# Diagnosed  ",value=as.vector(diagnosedCounts)),
      data.frame(time=times,var="Incidence (Base Case)  ",value=results[['base case']]$allInc),
      data.frame(time=times,var="Incidence (Upper Bound)  ",value=results[['upper bound']]$allInc)
    )
    dmax <- max(d$value)

    pAll <- ggplot(d,aes(x=time,y=value,linetype=var))  +   
      geom_line(aes(alpha=var)) +
      geom_point(aes(color=var)) + 
      theme_bw() + 
      scale_alpha_manual(values=c(.5,1,1),name="") + 
      scale_color_hue(name="") + 
      scale_linetype_manual(name="",values=c(3,1,2)) + 
      xlab("Time") + ylab("Counts") + ylim(c(0,dmax)) +
      theme(legend.position="bottom")

    # Undiagnosed counts
    d1 <- rbind(
      data.frame(time=times,var="Undiagnosed (Base Case)  ",value=results[['base case']]$undiag),
      data.frame(time=times,var="Undiagnosed (Upper Bound)  ",value=results[['upper bound']]$undiag)
    )
    d1max <- max(d1$value)
    d1min <- min(d1$value)

    cols <- hue_pal()(3)[-1]
    pUndiag <- ggplot(d1,aes(x=time,y=value,linetype=var))  +   
      geom_line() +
      geom_point(aes(color=var)) + 
      theme_bw() + 
      scale_color_manual(name="",values=cols) + 
      scale_linetype(name="") + 
      xlab("Time") + ylab("# Undiagnosed HIV+") + ylim(c(d1min,d1max)) +
      theme(legend.position="bottom")

    sumstats <- ddply(rbind(d,d1), .(var), function(x) c(summary(x$value)))

    return(list(stats=sumstats,
                statsAll=d,
                plotAll=pAll,
                plotUndiag=pUndiag,
                statsUndiag=d1))
}

######################################################################
# SUMMARIZE RESULTS, COMBINING NOIMPUTE + IMPUTE
######################################################################
summarize_runBackCalc_combined = function(results,
                                 diagnosedCounts,
                                 times) {

    d <- rbind(
        data.frame(time=times,
                   group='Diagnoses and Incidence',
                   var='# Diagnosed',
                   value=as.vector(diagnosedCounts)),
        data.frame(time=times,
                   group='Diagnoses and Incidence',
                   var='Base Case', 
                   value=as.vector(results[['noimpute']][['base case']]$allInc)),
        data.frame(time=times,
                   group='Diagnoses and Incidence',
                   var='Worst Case Obs',
                   value=as.vector(results[['noimpute']][['upper bound']]$allInc)),
        data.frame(time=times,
                   group='Diagnoses and Incidence',
                   var='Worst Case Miss',
                   value=as.vector(results[['impute']][['upper bound']]$allInc)),
        data.frame(time=times,
                   group='Undiagnosed Cases',
                   var='Base Case', 
                   value=as.vector(results[['noimpute']][['base case']]$undiag)),
        data.frame(time=times,
                   group='Undiagnosed Cases',
                   var='Worst Case Obs',
                   value=as.vector(results[['noimpute']][['upper bound']]$undiag)),
        data.frame(time=times,
                   group='Undiagnosed Cases',
                   var='Worst Case Miss',
                   value=as.vector(results[['impute']][['upper bound']]$undiag)))

    p <- ggplot(d,aes(x=time,y=value,group=var)) +
      geom_line(aes(alpha=var, color=var)) +
      geom_point(aes(color=var)) +
      scale_alpha_manual(values=c(.5,1,1,1),name="") + 
      scale_linetype_manual(name="",values=c(3,1,2,2)) + 
      theme_bw()+
      theme(legend.position='bottom',axis.text.x=element_text(angle=90)) + 
      scale_color_hue(name="") + 
      scale_x_continuous(breaks=seq(min(d$time),max(d$time),by=2))+
      xlab("Time") + ylab("Counts") +
      facet_grid(group~., scales='free_y')

      return(p)
}

######################################################################
# FORMAT SUMMARIZED RESULTS
######################################################################

# Insert the 'stats' object created at the end of run_main, and get
# out selected rows with particular labels
format_stats <- function(stats) {
  stats <- stats[c(1,7,8,3,9,10,5),]
  stats <- transform(stats,
                     var=as.character(var),
                     imputed=NULL)
  stats$var[3] <- 'Incidence (Worst Case Obs)'
  stats$var[4] <- 'Incidence (Worst Case Miss)'
  stats$var[6] <- 'Undiagnosed (Worst Case Obs)'
  stats$var[7] <- 'Undiagnosed (Worst Case Miss)'
  stats$var <- gsub('[[:space:]]+$','',stats$var)
  colnames(stats) <- gsub('\\.', ' ', colnames(stats))
  colnames(stats) <- gsub('^X', '', colnames(stats))
  colnames(stats) <- gsub('var', '', colnames(stats))
  return(stats)
}


######################################################################
# CATCHING ERRORS
######################################################################

# From demo(error.catching)
tryCatch.W.E <- function(expr) {
     W <- NULL
     w.handler <- function(w){ # warning handler
       W <<- w
       invokeRestart("muffleWarning")
         }
     list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler), warning = W)
}

######################################################################
# COMPUTE TRUE PREVALENCE
######################################################################

# thiscategory and thisgroup:
# These should match the Category and Group variables in the 
# prevalence_file

# results_file and prevalence_file are within dir_main

# if aggregate!=NULL, expect a vector for thisgroup and results_file, 
# and the value for aggregate should be the name of the summed groups, 
# e.g. 'Total-weighted'
true_prevalence = function(thiscategory,
                             thisgroup,
                             thispop,
                             results_file,
                             prevalence_file,
                             aggregate=NULL,
                             dir_main='/Users/jeanette/Dropbox/School/PhD/HIV_WA',
                             returntp=FALSE,
                             returnmid=FALSE,
                             use_imputed_results='No',
                             otherID='') {



    # SETUP
    library(plyr)
    library(reshape)

    if (!is.null(aggregate)) {
        results <- vector(mode='list', length=length(thisgroup))
        for (g in 1:length(thisgroup)) {
            results[[g]] <- true_prevalence(thiscategory,
                                            thisgroup[g],
                                            thispop,
                                            results_file[g],
                                            prevalence_file,
                                            aggregate=NULL,
                                            returntp=TRUE)
        }

        results <- do.call('rbind', results)
        summed <- ddply(subset(results, select=-Group), 
                        .(Population, Category, Year, Case),
                        numcolwise(sum))
        summed <- within(summed, {
                           UndiagPercMin=round(100*UndiagQtrMin/TruePrevMin,1)
                           UndiagPercAvg=round(100*UndiagQtrAvg/TruePrevAvg,1)
                           UndiagPercMax=round(100*UndiagQtrMax/TruePrevMax,1)
                           Case = gsub('Undiagnosed ', '', Case)
                           Case = gsub('\\(', '', Case)
                           Case = gsub('\\)', '', Case)
                    })
        summed <- arrange(summed, Year, Case)
        summed$Category <- aggregate
        summed$Group <- paste(thisgroup, collapse='+')
        summed <- summed[,c('Population', 'Category', 'Group', 'Year', 
                    'Case', 'Reported_Prev', 
                    rev(grep('Qtr', colnames(summed), value=TRUE)), 
                    rev(grep('True', colnames(summed), value=TRUE)), 
                    rev(grep('Perc', colnames(summed), value=TRUE)))]
                            
        # Save

        write.csv(summed, 
                  file.path(dir_main, 'analysis_WA', 'results', 
                                paste0('true_prevalence_', aggregate, 
                                       '_', unique(summed$Group), '.csv')),
                  row.names=FALSE)

        if (returntp) return(summed)

    } else {

        results <- read.csv(file.path(dir_main, results_file), header=TRUE)

        prevalence <- subset(read.csv(file.path(dir_main, prevalence_file), 
                                      header=TRUE), select=-c(Notes))

        # Format results and prevalence

        # Aggregate quarterly counts into yearly
        results <- transform(results, time=floor(time))
        undiag <- cbind(aggregate(formula=value~time+var+imputed, 
                                    data=results, FUN=min), 
                          UndiagQtrAvg=aggregate(formula=value~time+var+imputed, 
                                           data=results, FUN=mean)$value,
                          UndiagQtrMax=aggregate(formula=value~time+var+imputed, 
                                           data=results, FUN=max)$value)
        undiag <- rename(undiag, c('time'='Year', 'var'='Case', 'value'='UndiagQtrMin'))
        undiag <- subset(undiag[grepl('Undiagnosed', undiag$Case),],
                         imputed==use_imputed_results)

        # Format prevalence to have year long
        prevalence <- melt(prevalence)
        prevalence <- within(prevalence, {
                             time=as.numeric(gsub('y', '', variable))
                             variable=NULL
                         })
        # Restrict to group of interest
        prevalence <- subset(prevalence, Category==thiscategory & Group==thisgroup)
        prevalence <- rename(prevalence, c('time'='Year', 'value'='Reported_Prev'))

        # True prevalence and percent undiagnosed

        tp <- merge(prevalence, undiag, all=TRUE)
        tp <- within(tp, {
                           Population=thispop
                           UndiagQtrMax=round(UndiagQtrMax,1)
                           UndiagQtrAvg=round(UndiagQtrAvg,1)
                           UndiagQtrMin=round(UndiagQtrMin,1)
                           TruePrevMin=UndiagQtrMin+Reported_Prev 
                           TruePrevAvg=UndiagQtrAvg+Reported_Prev 
                           TruePrevMax=UndiagQtrMax+Reported_Prev 
                           UndiagPercMin=round(100*UndiagQtrMin/TruePrevMin,1)
                           UndiagPercAvg=round(100*UndiagQtrAvg/TruePrevAvg,1)
                           UndiagPercMax=round(100*UndiagQtrMax/TruePrevMax,1)
                           Case = gsub('Undiagnosed ', '', Case)
                           Case = gsub('\\(', '', Case)
                           Case = gsub('\\)', '', Case)
                           imputed = NULL
                    })
        tp <- arrange(tp, Year, Case)
        tp <- tp[,c('Population', 'Category', 'Group', 'Year', 
                    'Case', 'Reported_Prev', 
                    rev(grep('Qtr', colnames(tp), value=TRUE)), 
                    rev(grep('True', colnames(tp), value=TRUE)), 
                    rev(grep('Perc', colnames(tp), value=TRUE)))]
                            
        # Save

        write.csv(tp, file.path(dir_main, 'analysis_WA', 'results', 
                                paste0('true_prevalence_', thiscategory, 
                                       '_', thisgroup, otherID, '.csv')),
                  row.names=FALSE)

        if (returntp) return(tp)
    }
}

######################################################################
# GRAPH TRUE PREVALENCE
######################################################################
# See true_prevalence_report.Rnw/pdf for output
# This is only equipped for one Base Case and one Upper Bound, so 
# the input files must only have imputed='No' OR imputed='Yes' but not both...OR, it looks like you could use this_subset to subset the data

trueprev_figure <- function(files,
                        dir_files,
                        suffix,
                        this_subset=NULL,
                        group=NULL) {

    # Combine files if necessary
    for (i in 1:length(files)) {
        tp <- read.csv(file.path(dir_files, files[i]))
        if (i==1) tpall <- tp
        else tpall <- rbind(tpall, tp)
    }
    
    if (is.null(group)) tpall$Panel <- 'All' else tpall$Panel <- tpall[,group]

    # Fill in NAs for Panel-Years that don't exist, in order
    # keep all bars the same width even when there's missing data
    # http://stackoverflow.com/questions/11020437/consistent-width-for-geom-bar-in-the-event-of-missing-data
    template <- expand.grid(Year=unique(tpall$Year), Panel=unique(tpall$Panel))
    tpall <- merge(tpall, template, all=TRUE)

    # Now format
    tpall <- subset(tpall,
                     select=c(Population, Category, Group, Panel,
                              Year, Case,
                              UndiagQtrAvg,
                              TruePrevAvg,
                              UndiagPercAvg))
    tpall <- rename(tpall,
                     c('UndiagQtrAvg'='Estimated Undiagnosed',
                       'TruePrevAvg'='Estimated PLWHA',
                       'UndiagPercAvg'='Estimated % Undiagnosed'))
    tpall <- melt(tpall,
                   id.vars=c('Population', 'Category', 
                             'Group', 'Year', 'Case', 'Panel'))
    tpall <- dcast(tpall,
                    formula=(Panel+Population+Category+Group+Year+variable~Case))

    if (!is.null(this_subset)) tpall <- subset(tpall,
                                               variable==this_subset)

    height <- length(unique(tpall$variable))

    pl <- ggplot(tpall,
                 aes(x=as.factor(Year), y=`Base Case  `,
                     ymin=`Base Case  `,
                     ymax=`Upper Bound  `,
                     colour=Panel,
                     fill=Panel)) +
          scale_x_discrete(limits=c(as.character(min(tp$Year):
                                    max(tp$Year))), name='') +
          scale_y_continuous(name='') + 
          scale_fill_discrete(name='') + 
          guides(colour=FALSE) + 
          geom_crossbar(width=0.5, position=position_dodge(width=1.05)) +
          theme_bw() + 
          facet_grid(variable~., scales='free_y') # facet_wrap won't work

    if (is.null(group)) pl <- pl + guides(fill=FALSE)

    ggsave(plot=pl, 
           height=height*3,
           file=file.path(dir_files, 
                                   paste0('true_prevalence_',
                                          suffix, '.pdf')))

    return(pl)
} 


######################################################################
# GRAPH TRUE PREVALENCE - IMPUTED+NON-IMPUTED
######################################################################
# See true_prevalence_report.Rnw/pdf for output
# Uses different point types to show the 4 results and colors to 
# offset populations (e.g. MSM vs non-MSM)

# fileIDs: creates a new variable in files with element 1 as the name of the variable and remaining elements as the value for the files, in the same order as the file names
trueprev_figure_4cases <- function(files,
                                   fileIDs, 
                                   dir_files, 
                                   suffix, 
                                   years,
                                   this_subset=NULL,
                                   group=NULL) {

    # Combine files if necessary
    for (i in 1:length(files)) {
        tp <- read.csv(file.path(dir_files, files[i]))
        tp[,fileIDs[1]] <- fileIDs[i+1]
        if (i==1) tpall <- tp
        else tpall <- rbind(tpall, tp)
    }
    
    if (is.null(group)) tpall$Panel <- 'All' else tpall$Panel <- tpall[,group]

    # Subset by year
    tpall <- tpall[tpall$Year%in%years,]

    # Fill in NAs for Panel-Years that don't exist, in order
    # keep all bars the same width even when there's missing data
    # http://stackoverflow.com/questions/11020437/consistent-width-for-geom-bar-in-the-event-of-missing-data
    template <- expand.grid(Year=unique(tpall$Year), Panel=unique(tpall$Panel))
    tpall <- merge(tpall, template, all=TRUE)

    # Now format
    tpall <- subset(tpall,
                     select=c(Population, Category, Group, Panel,
                              Year, Case, `Used Missing`,
                              UndiagQtrAvg,
                              TruePrevAvg,
                              UndiagPercAvg))
    tpall <- rename(tpall,
                     c('UndiagQtrAvg'='Estimated Undiagnosed',
                       'TruePrevAvg'='Estimated PLWHA',
                       'UndiagPercAvg'='Estimated % Undiagnosed'))
    tpall <- melt(tpall,
                   id.vars=c('Population', 'Category', 'Used Missing',
                             'Group', 'Year', 'Case', 'Panel'))

    tpall <- within(tpall, {
                    Temp = ifelse(`Used Missing`=='Yes',
                                  ' (Missing)', ' (Observed)')
                    Case = paste0(gsub("^\\s+|\\s+$", "", Case), Temp)
                    Case = factor(Case, 
                                  levels=c('Base Case (Observed)',
                                           'Upper Bound (Observed)',
                                           'Base Case (Missing)',
                                           'Upper Bound (Missing)'),
                                  labels=c('Base Case (Observed)',
                                           'Upper Bound (Observed)',
                                           'Base Case (Missing)',
                                           'Upper Bound (Missing)'))
                      })

    if (!is.null(this_subset)) tpall <- subset(tpall,
                                               variable==this_subset)

    height <- length(unique(tpall$variable))

    pl <- ggplot(tpall,
                 aes(x=Panel, y=value,
                     colour=Case,
                     shape=Case))+
          scale_x_discrete(name='') +
          scale_y_continuous(name='') + 
          geom_point() + 
          theme_bw() + 
          facet_grid(variable~., scales='free_y') # facet_wrap won't work

    ggsave(plot=pl, 
           height=height*3,
           file=file.path(dir_files, 
                                   paste0('true_prevalence_',
                                          suffix, '.pdf')))

    return(pl)
} 
