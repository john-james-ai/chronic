#==============================================================================#
#                               analyzeRq3.R                                   #
#==============================================================================#
#'
#' analyzeRq3
#'
#' \code{analyzeRq3(brfss)}{Performs analyses for research question #3,
#' To what degree does depression with co-occurring chronic illness affect
#' productivity, vis-a-vis that of diagnoses of depression or chronic illness
#' separately?}
#'
#' This function analyzes the effects of depression and chronic illness on the
#' response variable,  the number of sick days reported in the 30 days
#' preceding the survey.  This analysis is conducted in the following five
#' sections.
#'
#' @section Acquire Data:
#' This section extracts the number of sick days and the sick days range
#' variable, as well as the depression and chronic illnesses indicator
#' variables.
#'
#' @section Create Marginal and Joint Contingency Tables:
#' This section renders the tables that illuminate the marginal and joint
#' effects of depression and chronic illness on the number of sick days
#' reported.  Descriptive statistics of sick days are also reported
#' for both depression and chronic illness, as well as their joint
#' effects on the number of sick days reported.
#'
#' @section Plots:
#' In this section, frequency and proportion bar plots, box plots, violin
#' plots and histograms are rendered to illuminate the distribution of
#' sick days vis-a-vis chronic illness and depression diagnoses.
#'
#' @section Statistical Tests:
#' In this section, the non-parametric Wilcoxin signed-rank and the
#' the Kolmogorov–Smirnov tests were administered to ascertain the
#' effect of depression and chronic illness on the number of sick days
#' reported.  The Wilcox test compared the mean number of sick days
#' reported for both depression and chronic disease diagnoses for
#' statistically significant differences.  The Komogorov-Smirnov tests
#' were conducted pairwise to reveal relative effects of depression
#' and chronic illness on sick days reported.
#'
#' @param brfss Data frame of preprocessed 2013 Behavioral Risk Factor
#' Surveillance System (BRFSS) telephone survey.
#'
#' @return analysis List containing frequency and proportion data, plots,
#'     and statistical tests. The variables are as follows:
#'     \itemize{
#'      \itemize{Data Tables}{
#'       \item{depressionFreqTbl}{Table of frequencies of depression diagnoses}
#'       \item{depressionPropTbl}{Table of proportions of depression diagnoses}
#'       \item{chronicFreqTbl}{Table of frequencies of chronic disease diagnoses}
#'       \item{chronicPropTbl}{Table of proportions of chronic disease diagnoses}
#'       \item{interactionFreqTbl}{Table of frequencies of the interaction between chronic illness and depression diagnoses}
#'       \item{interactionPropTbl}{Table of proportions of the interaction between chronic illness and depression diagnoses}
#'      }
#'      \itemize{Data Frames}{
#'       \item{depressionData}{Depression frequency data in data frame format}
#'       \item{chronicData}{Chronic disease frequency data in data frame format}
#'       \item{interactionData}{Interaction between depression and chronic illness frequency data in data frame format}
#'     }
#'     \itemize{Statistics}{
#'      \item{depression}{Descriptive statistics for sick days vis-a-vis depression diagnoses}
#'      \item{chronic}{Descriptive statistics for sick days vis-a-vis chronic illness diagnoses}
#'      \item{interaction}{Descriptive statistics for sick days vis-a-vis the depression / chronic illness interaction variable}
#'      \item{allChronic}{Descriptive statistics for sick days vis-a-vis a diagnoses of any chronic illness}
#'     }
#'     \itemize{Plots}{
#'      \item{depressionFreqBar}{The depression frequency bar plot}
#'      \item{depressionPropBar}{The depression proportion bar plot}
#'      \item{depressionHist1}{A histogram of the distribution of sick days vis-a-vis a diagnosis of depression}
#'      \item{depressionHist2}{A histogram of the distribution of one or more sick days reported vis-a-vis a diagnosis of depression}
#'      \item{depressionViolin}{A violin plot of the distribution of sick days vis-a-vis a diagnosis of depression}
#'      \item{depressionBox}{A box plot of the distribution of sick days vis-a-vis a diagnosis of depression}
#'      \item{chronicFreqBar}{The chronic illness diagnosis frequency bar plot}
#'      \item{chronicPropBar}{The chronic illness diagnosis proportion bar plot}
#'      \item{chronicHist1}{A histogram of the distribution of sick days vis-a-vis a diagnosis of chronic illness}
#'      \item{chronicHist2}{A histogram of the distribution of one or more sick days reported vis-a-vis a diagnosis of chronic illness}
#'      \item{chronicViolin}{A violin plot of the distribution of sick days vis-a-vis a diagnosis of chronic illness}
#'      \item{chronicBox}{A box plot of the distribution of sick days vis-a-vis a diagnosis of chronic illness}
#'      \item{allChronic}{A box plot of the distribution of sick days vis-a-vis a diagnosis of of any chronic illness}
#'      \item{interactionFreqBar}{The depression / chronic illness interaction variable frequency bar plot of sick }
#'      \item{interactionPropBar}{The depression / chronic illness interaction variable proportion bar plot}
#'      \item{interactionHist1}{A histogram of the distribution of sick days vis-a-vis the depression / chronic illness interaction variable}
#'      \item{interactionHist2}{A histogram of the distribution of one or more sick days reported vis-a-vis the depression / chronic illness interaction variable}
#'      \item{interactionViolin}{A violin plot of the distribution of sick days vis-a-vis the depression / chronic illness interaction variable}
#'      \item{interactionBox}{A box plot of the distribution of sick days vis-a-vis the depression / chronic illness interaction variable}
#'      \item{allinteraction}{A box plot of the distribution of sick days vis-a-vis the depression / chronic illness interaction variable}
#'     }
#'     \itemize{tests}{
#'      \item{depressionTest}{The Wilcoxin ranked-sums test of equal means with and without depression}
#'      \item{depressionEffect}{The Kolmogorov-Smirnov test of the effect of depression on sick days reported}
#'      \item{chronicTest}{The Wilcoxin ranked-sums test of equal means with and without a diagnosis of chronic illness}
#'      \item{chronicEffect}{The Kolmogorov-Smirnov test of the effect of chronic illness on sick days reported}
#'      \item{interactionTest}{The Wilcoxin ranked-sums test of equal means vis-a-vis the depression / chronic illness interaction variable}
#'      \item{interactionModel}{The linear model that relates sick days to the values of the interaction variable}
#'      \item{pairwise}{The pairwise Kolmogorov-Smirnov test of the effect of depression and chronic illness on sick days reported}
#'     }
#'  }
#'
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
analyzeRq3 <- function(brfss) {
  #---------------------------------------------------------------------------#
  #                                Get Data                                   #
  #---------------------------------------------------------------------------#
  # Get complete cases for interaction analysis and create interaction variable
  interactionData <- brfss %>%
    filter(!is.na(SickDays) & !is.na(Depression) & !is.na(Chronic) & SickDays < 32) %>%
    select(SickDays, Depression,  Chronic, SickDaysInd)

  interactionData$DepressionChronic <-
    interaction(interactionData$Depression, interactionData$Chronic)
  interactionDataPhd <- interactionData %>% filter(SickDays > 0)

  # Get depression data
  depressionData <- brfss %>% filter(!is.na(SickDays) &
                                       !is.na(Depression) & SickDays < 32) %>%
    select(SickDays, Depression, SickDaysInd)
  depressionDataPhd <- depressionData %>% filter(SickDays > 0)

  # Get chronic data
  chronicData <- brfss %>%  filter(!is.na(SickDays) &
                                   !is.na(Chronic) & SickDays < 32) %>%
    select(SickDays, Chronic, SickDaysInd)
  chronicDataPhd <- chronicData %>% filter(SickDays > 0)

  # Get data for all chronic disease, including depression
  allChronicData <- brfss %>%  filter(!is.na(SickDays) & Depression == 'No' &
                                        SickDays < 32 &
                                        !is.na('Heart Attack') & !is.na(Chronic) &
                                        Chronic == 'Yes' &
                                        !is.na('Angina Or Coronary Heart Disease') &
                                        !is.na(Stroke) & !is.na(Asthma) &
                                        !is.na('Skin Cancer') & !is.na('Cancer (Other)') &
                                        !is.na(COPD) & !is.na(Arthritis) &
                                        !is.na('Kidney Disease') & !is.na(Diabetes)) %>%
    select(SickDays, Depression, SickDaysInd, 'Heart Attack',
           'Angina Or Coronary Heart Disease', Stroke, Asthma, 'Skin Cancer',
           'Cancer (Other)', COPD, Arthritis, Diabetes, 'Kidney Disease')
  #---------------------------------------------------------------------------#
  #             Create Depression Marginal Contingency Tables                 #
  #---------------------------------------------------------------------------#
  # Create Tables for Analysis of Effect of Depression on Productivity        #
  #---------------------------------------------------------------------------#
  # Create 2-Way Marginal Contingency Tables on Depression and Sick Days
  depressionTbl <- with(depressionData, table(Depression, SickDaysInd))
  depressionFreqTbl <- ftable(depressionTbl)
  depressionFreqDf <- as.data.frame(depressionFreqTbl)
  depressionPropTbl <- ftable(prop.table(depressionTbl, 1))
  depressionPropDf <- as.data.frame(depressionPropTbl)

  # Format 2-Way Depression Marginal Frequency Contingency Tables for plotting
  depressionFreqDf <- depressionFreqDf %>% arrange(Depression,
                                                   desc(SickDaysInd)) %>%
    group_by(Depression) %>%
    mutate(cumFreq = cumsum(Freq), pos = cumFreq - 0.5 * Freq)
  # Format 2-Way Depression Marginal Proportion Contingency Tables for plotting
  depressionPropDf <- depressionPropDf %>% arrange(Depression,
                                                   desc(SickDaysInd)) %>%
    group_by(Depression) %>%
    mutate(pct = round(Freq * 100, 0), cumPct = cumsum(pct),
           pos = cumPct - 0.5 * pct)


  #---------------------------------------------------------------------------#
  #           Create Chronic Illness Marginal Contingency Tables              #
  #---------------------------------------------------------------------------#
  # Create Tables for Analysis of Effect of Chronic Illness on Productivity   #
  #---------------------------------------------------------------------------#
  # Create 2-Way Marginal Contingency Tables on Chronic Ilness and Sick Days
  chronicTbl <- with(chronicData, table(Chronic, SickDaysInd))
  chronicFreqTbl <- ftable(chronicTbl)
  chronicFreqDf <- as.data.frame(chronicFreqTbl)
  chronicPropTbl <- ftable(prop.table(chronicTbl, 1))
  chronicPropDf <- as.data.frame(chronicPropTbl)
  # Format 2-Way Chronic Marginal Frequency Contingency Tables for plotting
  chronicFreqDf <- chronicFreqDf %>% arrange(Chronic,
                                             desc(SickDaysInd)) %>%
    group_by(Chronic) %>%
    mutate(cumFreq = cumsum(Freq), pos = cumFreq - 0.5 * Freq)

  # Format 2-Way Chronic Marginal Proportional Contingency Tables for plotting
  chronicPropDf <- chronicPropDf %>% arrange(Chronic,
                                             desc(SickDaysInd)) %>%
    group_by(Chronic) %>%
    mutate(pct = round(Freq * 100, 0), cumPct = cumsum(pct),
           pos = cumPct - 0.5 * pct)


  #---------------------------------------------------------------------------#
  #       Create Depression and Chronic Illness Contingency Tables            #
  #---------------------------------------------------------------------------#
  # Create the 2X2 Depression and Chronic Illnesses Interaction Tables        #
  #---------------------------------------------------------------------------#
  # Create Joint Contingency Tables on Depression and Chronic Illness
  interactionTbl <- with(interactionData, table(Depression, Chronic, SickDaysInd))
  interactionFreqTbl <- ftable(interactionTbl)
  interactionFreqDf <- as.data.frame(interactionFreqTbl)
  interactionPropTbl <- ftable(prop.table(interactionTbl, 1))
  interactionPropDf <- as.data.frame(interactionPropTbl)

  # Format 2-Way Depression Marginal Frequency Contingency Tables for plotting
  interactionFreqDf <- interactionFreqDf %>% arrange(Depression, Chronic,
                                                   desc(SickDaysInd)) %>%
    group_by(Depression) %>%
    mutate(cumFreq = cumsum(Freq), pos = cumFreq - 0.5 * Freq)

  # Format 2-Way Depression Marginal Proportion Contingency Tables for plotting
  interactionPropDf <- interactionPropDf %>% arrange(Depression, Chronic,
                                                   desc(SickDaysInd)) %>%
    group_by(Depression) %>%
    mutate(pct = round(Freq * 100, 0), cumPct = cumsum(pct),
           pos = cumPct - 0.5 * pct)

  #---------------------------------------------------------------------------#
  #                         Get Summary Statistics                            #
  #---------------------------------------------------------------------------#
  # Get mean of restricted activity days by depression
  depressionMeans <- aggregate(SickDays ~ Depression, interactionData, mean)
  depressionMeansPhd <- aggregate(SickDays ~ Depression, interactionDataPhd, mean)

  # Get mean of restricted activity days by chronic
  chronicMeans <- aggregate(SickDays ~ Chronic, interactionData, mean)
  chronicMeansPhd <- aggregate(SickDays ~ Chronic, interactionDataPhd, mean)

  # Get mean of restricted activity days by depression  and chronic illness
  interactionMeans <- aggregate(SickDays ~ Depression + Chronic, interactionData, mean)
  interactionMeansPhd <- aggregate(SickDays ~ Depression + Chronic, interactionDataPhd, mean)

  #---------------------------------------------------------------------------#
  # Summary stats function
  getStats <- function(theData, varName, eVar, value) {
    stats <- psych::describe(theData)
    stats <- data.frame(stats, row.names = NULL)
    ci <- pastecs::stat.desc(theData)[11,1]
    ds <- data.frame(Variable = varName,
                     Value = value,
                     N = stats$n,
                     Min = stats$min,
                     Lower = quantile(theData, .25, na.rm = TRUE),
                     Median = stats$median,
                     Mode = getMode(as.vector(theData$SickDays)),
                     Mean = round(stats$mean, 2),
                     CI = round(ci, 3),
                     Upper = quantile(theData, .75, na.rm = TRUE),
                     Max = stats$max,
                     Range = stats$range,
                     Total = sum(theData, na.rm = TRUE),
                     SD = round(stats$sd, 2),
                     SE = round(stats$se, 3),
                     Skew = round(stats$skew, 2),
                     Kurtosis = round(stats$kurtosis, 2))

    names(ds)[names(ds) == 'Value'] <- eVar
    return(ds)
  }

  #---------------------------------------------------------------------------#
  # Get summary stats of sick days by depression, chronic illness, and interaction
  values <- list('Yes', 'No')

  depressionStats <-
    data.table::rbindlist(lapply(seq_along(values), function(x) {
    getStats(depressionData %>% filter(Depression == values[[x]])
             %>% select(SickDays), 'SickDays', 'Depression', values[[x]])
    }))

  chronicStats <-
    data.table::rbindlist(lapply(seq_along(values), function(x) {
      getStats(chronicData %>% filter(Chronic == values[[x]])
               %>% select(SickDays), 'SickDays', 'Chronic', values[[x]])
    }))

  # Chronic Illness Stats
  depStats <- getStats(depressionData %>% filter(depressionData[,2] == 'Yes')
                      %>% select(SickDays), 'SickDays',  'Condition', 'Depression')
  haStats <- getStats(allChronicData %>% filter(allChronicData[,4] == 'Yes')
                      %>% select(SickDays), 'SickDays',  'Condition', 'Heart Attack')
  anginaStats <- getStats(allChronicData %>% filter(allChronicData[,5] == 'Yes')
                          %>% select(SickDays), 'SickDays', 'Condition',  'Angina or CHD')
  strokeStats <- getStats(allChronicData %>% filter(allChronicData[,6] == 'Yes')
                          %>% select(SickDays), 'SickDays',  'Condition', 'Stroke')
  asthmaStats <- getStats(allChronicData %>% filter(allChronicData[,7] == 'Yes')
                          %>% select(SickDays), 'SickDays',  'Condition', 'Asthma')
  skinStats <- getStats(allChronicData  %>% filter(allChronicData[,8] == 'Yes')
                        %>% select(SickDays), 'SickDays',  'Condition', 'Skin Cancer')
  cancerStats <- getStats(allChronicData  %>% filter(allChronicData[,9] == 'Yes')
                          %>% select(SickDays), 'SickDays',  'Condition', 'Cancer (Other)')
  copdStats <- getStats(allChronicData %>% filter(allChronicData[,10] == 'Yes')
                           %>% select(SickDays), 'SickDays',  'Condition', 'COPD')
  arthritisStats <- getStats(allChronicData %>% filter(allChronicData[,11] == 'Yes')
                             %>% select(SickDays), 'SickDays',  'Condition', 'Arthritis')
  diabetesStats <- getStats(allChronicData %>% filter(allChronicData[,12] == 'Yes')
                               %>% select(SickDays), 'SickDays',  'Condition', 'Diabetes')
  kidneyStats <- getStats(allChronicData %>% filter(allChronicData[,13] == 'Yes')
                          %>% select(SickDays), 'SickDays',  'Condition', 'Kidney Disease')
  allChronicStats <- rbind(depStats, haStats, anginaStats, strokeStats, asthmaStats, skinStats,
                               cancerStats, copdStats, arthritisStats, diabetesStats,
                               kidneyStats)
  allChronicStats <- allChronicStats[order(-allChronicStats$Mean),]

  values <- list('Yes.Yes', 'No.Yes', 'Yes.No', 'No.No')
  interactionStats <-
    data.table::rbindlist(lapply(seq_along(values), function(x) {
      getStats(interactionData %>% filter(DepressionChronic == values[[x]])
               %>% select(SickDays), 'SickDays', 'DepressionChronic', values[[x]])
    }))

  #---------------------------------------------------------------------------#
  #                       Create Depression Plots                             #
  #---------------------------------------------------------------------------#
  # Create Marginal Frequency Bar Plot of Sick Days Indicator by Depression
  depressionFreqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Depression, y = Freq, fill = SickDaysInd),
             data = depressionFreqDf, stat = 'identity') +
    ggplot2::geom_text(data = depressionFreqDf, ggplot2::aes(x = Depression, y = pos,
                                           label = Freq),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens',
                      name = "Sick Days") +
    ggplot2::labs(title = "Frequency Distribution",
         x = "Diagnosis of Depression",
         y = 'Number Reporting Sick Days')

  #---------------------------------------------------------------------------#
  # Create Marginal Proportion Bar Plot of Sick Days Indicator by Depression
  depressionPropBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Depression, y = pct, fill = SickDaysInd),
             data = depressionPropDf, stat = 'identity') +
    ggplot2::geom_text(data = depressionPropDf, ggplot2::aes(x = Depression, y = pos,
                                           label = paste0(pct,"%")),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Sick Days") +
    ggplot2::labs(title = "Proportional Distribution",
         x = "Diagnosis of Depression",
         y = 'Percentage Reporting Sick Days')

  #---------------------------------------------------------------------------#
  # Create violin of sick days grouped by diagnosis of depression
  depressionViolin <- ggplot2::ggplot(depressionData,
                          ggplot2::aes(x = Depression, y = SickDays, fill = Depression)) +
    ggplot2::geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::labs(title = 'Violin Plot',fill = 'Depression') +
    ggplot2::ylab('Sick Days') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')


  #---------------------------------------------------------------------------#
  # Create box of sick days grouped by diagnosis of depression
  depressionBox <- ggplot2::ggplot(depressionData,
                             ggplot2::aes(x = Depression, y = SickDays, fill = Depression)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = 'Box Plot', fill = 'Depression') +
    ggplot2::ylab('Sick Days') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Plot a histogram of restricted activity days by depression diagnosis
  plotDepressionHist <- function(plotData, means) {

    # Create histogram of sick days grouped a diagnosis of depression
    hist <- ggplot2::ggplot(plotData,
                   ggplot2::aes(x=SickDays, color = Depression, fill = Depression)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), position = "identity", alpha = 0.6,
                     binwidth = 5) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = SickDays, color = Depression),
                 linetype = 'dashed') +
      ggplot2::scale_fill_brewer(palette = 'Accent') +
      ggplot2::scale_color_brewer(palette = 'Accent') +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::labs(title = "Density Sick Days Distribution by Diagnosis of Depression",
           x = "Sick Days", y = 'Density')

    return(hist)
  }
  depressionHist1 <- plotDepressionHist(depressionData, depressionMeans)
  depressionHist2 <- plotDepressionHist(depressionDataPhd, depressionMeansPhd)


  #---------------------------------------------------------------------------#
  #                     Create Chronic Illness Plots                          #
  #---------------------------------------------------------------------------#
  # Create Marginal Frequency Bar Plot of Sick Days Indicator by Chronic Illness
  chronicFreqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Chronic, y = Freq, fill = SickDaysInd),
             data = chronicFreqDf, stat = 'identity') +
    ggplot2::geom_text(data = chronicFreqDf, ggplot2::aes(x = Chronic, y = pos,
                                           label = Freq),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Sick Days") +
    ggplot2::labs(title = "Frequency Distribution",
         x = "Diagnosis of Chronic Illness",
         y = 'Number Sick Days')

  #---------------------------------------------------------------------------#
  # Create Marginal Proportion Bar Plot of Sick Days and Chronic Illness Indicators
  chronicPropBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Chronic, y = pct, fill = SickDaysInd),
             data = chronicPropDf, stat = 'identity') +
    ggplot2::geom_text(data = chronicPropDf, ggplot2::aes(x = Chronic, y = pos,
                                        label = paste0(pct,"%")),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Sick Days") +
    ggplot2::labs(title = "Proportional Distribution",
         x = "Diagnosis of Chronic Illness",
         y = 'Percentage Reporting Sick Days')

  #---------------------------------------------------------------------------#
  # Create violin plot  of sick days grouped by diagnosis of chronic illness
  chronicViolin <- ggplot2::ggplot(chronicData,
                       ggplot2::aes(x = Chronic, y = SickDays, fill = Chronic)) +
    ggplot2::geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::labs(title = 'Violin Plot', fill = 'Chronic Illness') +
    ggplot2::ylab('Sick Days') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Create box plot  of sick days grouped by diagnosis of chronic illness
  chronicBox <- ggplot2::ggplot(chronicData,
                          ggplot2::aes(x = Chronic, y = SickDays, fill = Chronic)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = 'Box Plot', fill = 'Chronic Illness') +
    ggplot2::ylab('Sick Days') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')


  #---------------------------------------------------------------------------#
  # Create histogram of restricted activity days by diagnosis of chronic illness
  plotChronicHist <- function(plotData, means) {

    # Create histogram of sick days grouped a diagnosis of chronic illness
    hist <- ggplot2::ggplot(plotData,
                   ggplot2::aes(x=SickDays, color = Chronic, fill = Chronic)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), position = "identity", alpha = 0.6,
                     binwidth = 5) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = SickDays, color = Chronic),
                 linetype = 'dashed') +
      ggplot2::scale_fill_brewer(palette = 'Accent') +
      ggplot2::scale_color_brewer(palette = 'Accent') +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::labs(title = "Sick Days by Chronic Diagnosis",
           x = "Sick Days", y = 'Density')

    return(hist)
  }
  chronicHist1 <- plotChronicHist(chronicData, chronicMeans)
  chronicHist2 <- plotChronicHist(chronicDataPhd, chronicMeansPhd)


  #---------------------------------------------------------------------------#
  #                      Create Interaction Plots                             #
  #---------------------------------------------------------------------------#
  # Create Marginal Frequency Bar Plot of Sick Days Indicator by Depression
  interactionFreqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Depression, y = Freq, fill = Chronic),
             data = interactionFreqDf, stat = 'identity') +
    ggplot2::geom_text(data = interactionFreqDf, ggplot2::aes(x = Depression, y = pos,
                                            label = Freq),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens',
                      name = "Diagnosis") +
    ggplot2::labs(title = 'Frequency Distribution',
         x = "Diagnosis of Depression",
         y = 'Diagnosis of Chronic Illness')

  #---------------------------------------------------------------------------#
  # Create Marginal Proportion Bar Plot of Sick Days Indicator by Depression
  interactionPropBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Depression, y = pct, fill = Chronic),
             data = interactionPropDf, stat = 'identity') +
    ggplot2::geom_text(data = interactionPropDf, ggplot2::aes(x = Depression, y = pos,
                                            label = paste0(pct,"%")),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens',
                      name = "Diagnosis") +
    ggplot2::labs(title = 'Proportional Distribution',
         x = "Diagnosis of Depression",
         y = 'Diagnosis of Chronic Illness')
  #---------------------------------------------------------------------------#
  # Plot histogram of restricted activity days by chronic illness and depression
  plotinteractionHist <- function(plotData, means) {

    # Create histogram of sick days by depression and chronic illness
    hist <- ggplot2::ggplot(plotData,
                   ggplot2::aes(x=SickDays,  fill = DepressionChronic)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), position = "identity", alpha = 0.6,
                     binwidth = 5) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = SickDays),
                 linetype = 'dashed') +
      ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
      ggplot2::scale_color_brewer(palette = 'Greens') +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::labs(title = 'Sick Days by Diagnoses of Depression and Chronic Illness',
           x = "Sick Days", y = 'Density')

    return(hist)
  }
  interactionHist1 <- plotinteractionHist(interactionData, interactionMeans)
  interactionHist2 <- plotinteractionHist(interactionDataPhd, interactionMeansPhd)

  #---------------------------------------------------------------------------#
  # Create Interaction Violin Plot
  interactionViolin <- ggplot2::ggplot(interactionData,
                           ggplot2::aes(x = DepressionChronic, y = SickDays, fill = DepressionChronic)) +
    ggplot2::geom_violin() +
    ggplot2::labs(title = "Violin Plot",
         x = 'Depression / Chronic Illness',
         y = 'Sick Days') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::stat_summary(fun.y = mean, geom = 'point') +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Create Interaction Box Plot
  interactionBox <- ggplot2::ggplot(interactionData,
                              ggplot2::aes(x = DepressionChronic, y = SickDays, fill = DepressionChronic)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Box Plot",
         x = 'Depression / Chronic Illness',
         y = 'Sick Days') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::stat_summary(fun.y = mean, geom = 'point') +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Create Mean Sick Days by Chronic Illness Bar Plot
  allChronicBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Condition, y = Mean, fill = Condition),
             data = allChronicStats, stat = 'identity') +
    ggplot2::geom_text(data = allChronicStats, ggplot2::aes(x = Condition,  y = Mean/2,
                                          label = Mean),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'PRGn',
                      name = "Diagnosis") +
    ggplot2::labs(title = 'Mean Sick Days by Condition',
         y = 'Sick Days')

  #---------------------------------------------------------------------------#
  # Create model for interaction plot
  interactionModel <- lm(SickDays ~ Depression + Chronic + Depression*Chronic,
                         data = interactionData)

  #---------------------------------------------------------------------------#
  #                           Conduct Tests                                   #
  #---------------------------------------------------------------------------#
  # Conduct Wilcox and effect tests for depression on sick days
  depressionTest <- wilcox.test(SickDays ~ Depression, data = depressionData,
                                conf.int = TRUE)
  yesGroup <- depressionData %>% filter(Depression == 'Yes')
  noGroup <- depressionData %>% filter(Depression == 'No')
  depressionEffect <- ks.test(yesGroup$SickDays, noGroup$SickDays)

  # Conduct Wilcox and effect tests for chronic illness on sick days
  chronicTest <- wilcox.test(SickDays ~ Chronic, data = chronicData,
                             conf.int = TRUE)
  yesGroup <- chronicData %>% filter(Chronic == 'Yes')
  noGroup <- chronicData %>% filter(Chronic == 'No')
  chronicEffect <- ks.test(yesGroup$SickDays, noGroup$SickDays)

  # Conduct Kruskal and effect tests for depression n& chronic illness on sick days
  interactionTest <- kruskal.test(SickDays ~ DepressionChronic, data = interactionData)

  # Partition data by explanatory variable
  group1 <- interactionData %>% filter(Depression == 'Yes' & Chronic == 'Yes') %>% select(SickDays)
  group2 <- interactionData %>% filter(Depression == 'No' & Chronic == 'Yes') %>% select(SickDays)
  group3 <- interactionData %>% filter(Depression == 'Yes' & Chronic == 'No') %>% select(SickDays)
  group4 <- interactionData %>% filter(Depression == 'No' & Chronic == 'No') %>% select(SickDays)

  # Conduct pairwise wilcox teste of equal medians
  test1 <- wilcox.test(group1$SickDays, group2$SickDays, conf.int = TRUE)
  test2 <- wilcox.test(group2$SickDays, group3$SickDays, conf.int = TRUE)
  test3 <- wilcox.test(group3$SickDays, group4$SickDays, conf.int = TRUE)


  # Calculate effect sizes for each
  effect1 <- ks.test(group1$SickDays, group2$SickDays)
  effect2 <- ks.test(group2$SickDays, group3$SickDays)
  effect3 <- ks.test(group3$SickDays, group4$SickDays)

  # Format Test Results
  pairwise <- data.frame(Test = c(1:3),
                         A.Depression = c("Yes",'No', 'Yes'),
                         A.Chronic    = c("Yes",'Yes', 'No'),
                         B.Depression = c("No", 'Yes', 'No'),
                         B.Chronic    = c("Yes",'No', 'No'),
                         X2 = c(test1$statistic, test2$statistic, test3$statistic),
                         p = c(test1$p.value, test2$p.value, test3$p.value),
                         Estimate = c(test1$estimate, test2$estimate, test3$estimate),
                         Lower_CI = c(test1$conf.int[1], test2$conf.int[1], test3$conf.int[1]),
                         Upper_CI = c(test1$conf.int[2], test2$conf.int[2], test3$conf.int[2]),
                         Effect = c(effect1$statistic, effect2$statistic, effect3$statistic))


  #---------------------------------------------------------------------------#
  #                     Format and Return Results                             #
  #---------------------------------------------------------------------------#
  analysis <- list(
    tables = list(
      depressionFreqTbl = depressionFreqTbl,
      depressionPropTbl = depressionPropTbl,
      chronicTbl = chronicTbl,
      chronicFreqTbl = chronicFreqTbl,
      chronicPropTbl = chronicPropTbl,
      interactionTbl = interactionTbl,
      interactionFreqTbl = interactionFreqTbl,
      interactionPropTbl = interactionPropTbl
    ),
    dataFrames = list(
      depressionData = depressionData,
      chronicData = chronicData,
      interactionData = interactionData
    ),
    stats = list(
      depression = depressionStats,
      chronic = chronicStats,
      interaction = interactionStats,
      allChronic = allChronicStats
    ),
    plots = list(
      depressionFreqBar = depressionFreqBar,
      depressionPropBar = depressionPropBar,
      depressionHist1 = depressionHist1,
      depressionHist2 = depressionHist2,
      depressionViolin = depressionViolin,
      depressionBox = depressionBox,
      chronicFreqBar = chronicFreqBar,
      chronicPropBar = chronicPropBar,
      chronicHist1 = chronicHist1,
      chronicHist2 = chronicHist2,
      chronicViolin = chronicViolin,
      chronicBox = chronicBox,
      allChronic = allChronicBar,
      interactionFreqBar = interactionFreqBar,
      interactionPropBar = interactionPropBar,
      interactionHist1 = interactionHist1,
      interactionHist2 = interactionHist2,
      interactionViolin = interactionViolin,
      interactionBox = interactionBox
    ),
    tests = list(
      depressionTest = depressionTest,
      depressionEffect = depressionEffect,
      chronicTest = chronicTest,
      chronicEffect = chronicEffect,
      interactionTest = interactionTest,
      interactionModel = interactionModel,
      pairwise = pairwise
    )
  )
  return(analysis)

}
## ---- end
