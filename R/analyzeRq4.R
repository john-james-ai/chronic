#==============================================================================#
#                               analyzeRq4.R                                   #
#==============================================================================#
#'
#' analyzeRq4
#'
#' \code{analyzeRq4(brfss)}{Performs analyses for research question #4}
#'
#' Function that performs the analysis for research question #4:  Is there
#' a relationship between depression, chronic disease and the use of
#' health care services?

#'
#' @param brfss Data frame of preprocessed 2013 Behavioral Risk Factor
#' Surveillance System (BRFSS) telephone survey.
#' @return analysis List containing frequency and proportion data, plots,
#'     and statistical tests.
#' @author John James, \email{jjames@@dataference.com}
#' @export
analyzeRq4 <- function(brfss) {
  #---------------------------------------------------------------------------#
  #                                Get Data                                   #
  #---------------------------------------------------------------------------#

  # Get complete cases for interaction analysis and create interaction variable
  interactionData <- brfss %>%
    filter(!is.na(Visits) & !is.na(Depression) & !is.na(chronic)) %>%
    select(Visits, Depression,  Chronic)
  interactionData$DepressionChronic <-
    interaction(interactionData$Depression, interactionData$Chronic)
  interactionData5Plus <- interactionData %>% filter(Visits > 4)

  # Get depression data
  depressionData <- brfss %>% filter(!is.na(Visits) & !is.na(Depression)) %>%
    select(Visits, Depression)
  depressionData5Plus <- depressionData %>% filter(Visits > 4)

  # Get chronic data
  chronicData <- brfss %>%  filter(!is.na(Visits) & !is.na(chronic)) %>%
    select(Visits, Chronic)
  chronicData5Plus <- chronicData %>% filter(Visits > 4)

  # Get data for all chronic disease, including depression
  allChronicData <- brfss %>%  filter(!is.na(Visits) & Depression == 'No' &
                                        !is.na('Heart Attack') & !is.na(chronic) &
                                        Chronic == 'Yes' &
                                        !is.na('Angina Or Coronary Heart Disease') &
                                        !is.na(Stroke) & !is.na(Asthma) &
                                        !is.na('Skin Cancer') & !is.na('Cancer (Other)') &
                                        !is.na(COPD) & !is.na(Arthritis) &
                                        !is.na('Kidney Disease') & !is.na(Diabetes)) %>%
    select(Visits, Depression, SickDaysInd, 'Heart Attack',
           'Angina Or Coronary Heart Disease', Stroke, Asthma, 'Skin Cancer',
           'Cancer (Other)', COPD, Arthritis, Diabetes, 'Kidney Disease')


  #---------------------------------------------------------------------------#
  #                         Get Summary Statistics                            #
  #---------------------------------------------------------------------------#
  # Get mean of restricted activity days by depression
  depressionMeans <- aggregate(Visits ~ Depression, interactionData, mean)
  depressionMeans5Plus <- aggregate(Visits ~ Depression, interactionData5Plus, mean)

  # Get mean of restricted activity days by chronic
  chronicMeans <- aggregate(Visits ~ Chronic, interactionData, mean)
  chronicMeans5Plus <- aggregate(Visits ~ Chronic, interactionData5Plus, mean)

  # Get mean of restricted activity days by depression  and chronic illness
  interactionMeans <- aggregate(Visits ~ Depression + Chronic, interactionData, mean)
  interactionMeans5Plus <- aggregate(Visits ~ Depression + Chronic, interactionData5Plus, mean)

  #---------------------------------------------------------------------------#
  # Summary stats function
  getStats <- function(data, varName, eVar, value) {
    stats <- psych::describe(data)
    stats <- data.frame(stats, row.names = NULL)
    ci <- pastecs::stat.desc(data)[11,1]
    ds <- data.frame(Variable = varName,
                     Value = value,
                     N = stats$n,
                     Min = stats$min,
                     Lower = quantile(data, .25, na.rm = TRUE),
                     Median = stats$median,
                     Mode = getMode(as.vector(data$Visits)),
                     Mean = round(stats$mean, 2),
                     CI = round(ci, 3),
                     Upper = quantile(data, .75, na.rm = TRUE),
                     Max = stats$max,
                     Range = stats$range,
                     Total = sum(data, na.rm = TRUE),
                     SD = round(stats$sd, 2),
                     SE = round(stats$se, 3),
                     Skew = round(stats$skew, 2),
                     Kurtosis = round(stats$kurtosis, 2))

    names(ds)[names(ds) == 'Value'] <- eVar
    return(ds)
  }

  #---------------------------------------------------------------------------#
  # Get summary stats of Dr. visitsby depression, chronic illness, and interaction
  values <- list('Yes', 'No')

  depressionStats <-
    rbindlist(lapply(seq_along(values), function(x) {
    getStats(depressionData %>% filter(Depression == values[[x]])
             %>% select(Visits), 'Visits', 'Depression', values[[x]])
    }))


  chronicStats <-
    rbindlist(lapply(seq_along(values), function(x) {
      getStats(chronicData %>% filter(chronic == values[[x]])
               %>% select(Visits), 'Visits', 'Chronic', values[[x]])
    }))

  values <- list('Yes.Yes', 'No.Yes', 'Yes.No', 'No.No')
  interactionStats <-
    rbindlist(lapply(seq_along(values), function(x) {
      getStats(interactionData %>% filter(DepressionChronic == values[[x]])
               %>% select(Visits), 'Visits', 'DepressionChronic', values[[x]])
    }))

  # Chronic Illness Stats
  depStats <- getStats(depressionData %>% filter(depressionData[,2] == 'Yes')
                       %>% select(Visits), 'Visits',  'Condition', 'Depression')
  haStats <- getStats(allChronicData[,c(1,4)] %>% filter(allChronicData[,4] == 'Yes')
                      %>% select(Visits), 'Visits',  'Condition', 'Heart Attack')
  anginaStats <- getStats(allChronicData[,c(1,5)] %>% filter(allChronicData[,5] == 'Yes')
                          %>% select(Visits), 'Visits', 'Condition',  'Angina or CHD')
  strokeStats <- getStats(allChronicData[,c(1,6)] %>% filter(allChronicData[,6] == 'Yes')
                          %>% select(Visits), 'Visits',  'Condition', 'Stroke')
  asthmaStats <- getStats(allChronicData[,c(1,7)] %>% filter(allChronicData[,7] == 'Yes')
                          %>% select(Visits), 'Visits',  'Condition', 'Asthma')
  skinStats <- getStats(allChronicData[,c(1,8)] %>% filter(allChronicData[,8] == 'Yes')
                        %>% select(Visits), 'Visits',  'Condition', 'Skin Cancer')
  cancerStats <- getStats(allChronicData[,c(1,9)] %>% filter(allChronicData[,9] == 'Yes')
                          %>% select(Visits), 'Visits',  'Condition', 'Cancer (Other)')
  copdStats <- getStats(allChronicData[,c(1,10)] %>% filter(allChronicData[,10] == 'Yes')
                        %>% select(Visits), 'Visits',  'Condition', 'COPD')
  arthritisStats <- getStats(allChronicData[,c(1,11)] %>% filter(allChronicData[,11] == 'Yes')
                             %>% select(Visits), 'Visits',  'Condition', 'Arthritis')
  diabetesStats <- getStats(allChronicData[,c(1,12)] %>% filter(allChronicData[,12] == 'Yes')
                            %>% select(Visits), 'Visits',  'Condition', 'Diabetes')
  kidneyStats <- getStats(allChronicData[,c(1,13)] %>% filter(allChronicData[,13] == 'Yes')
                          %>% select(Visits), 'Visits',  'Condition', 'Kidney Disease')
  allChronicStats <- rbind(depStats, haStats, anginaStats, strokeStats, asthmaStats, skinStats,
                           cancerStats, copdStats, arthritisStats, diabetesStats,
                           kidneyStats)
  allChronicStats <- allChronicStats[order(-allChronicStats$Mean),]
  #---------------------------------------------------------------------------#
  #                       Create Depression Plots                             #
  #---------------------------------------------------------------------------#
  # Create violin of Dr. visitsgrouped by diagnosis of depression
  depressionViolin <- ggplot2::ggplot(depressionData,
                          ggplot2::aes(x = Depression, y = Visits, fill = Depression)) +
    ggplot2::geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::labs(fill = 'Depression') +
    ylab('Dr. Visits') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')


  #---------------------------------------------------------------------------#
  # Create box of Dr. visitsgrouped by diagnosis of depression
  depressionBox <- ggplot2::ggplot(depressionData,
                             ggplot2::aes(x = Depression, y = Visits, fill = Depression)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(fill = 'Depression') +
    ylab('Dr. Visits') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Plot a histogram of restricted activity days by depression diagnosis
  plotDepressionHist <- function(plotData, means) {

    # Create histogram of Dr. visitsgrouped a diagnosis of depression
    hist <- ggplot2::ggplot(plotData,
                   ggplot2::aes(x=Visits, color = Depression, fill = Depression)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), position = "identity", alpha = 0.6,
                     binwidth = 5) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = Visits, color = Depression),
                 linetype = 'dashed') +
      ggplot2::scale_fill_brewer(palette = 'Accent') +
      ggplot2::scale_color_brewer(palette = 'Accent') +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::labs(title = "Dr.Visits during Previous 12 Months",
           x = "Dr. Visits", y = 'Density')

    return(hist)
  }
  depressionHist1 <- plotDepressionHist(depressionData, depressionMeans)
  depressionHist2 <- plotDepressionHist(depressionData5Plus, depressionMeans5Plus)


  #---------------------------------------------------------------------------#
  #                     Create Chronic Illness Plots                          #
  #---------------------------------------------------------------------------#
  # Create violin plot  of Dr. visitsgrouped by diagnosis of chronic illness
  chronicViolin <- ggplot2::ggplot(chronicData,
                       ggplot2::aes(x = Chronic, y = Visits, fill = Chronic)) +
    ggplot2::geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::labs(fill = 'Chronic Illness') +
    ylab('Dr. Visits') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Create box plot  of Dr. visitsgrouped by diagnosis of chronic illness
  chronicBox <- ggplot2::ggplot(chronicData,
                          ggplot2::aes(x = Chronic, y = Visits, fill = Chronic)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(fill = 'Chronic Illness') +
    ylab('Dr. Visits') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')


  #---------------------------------------------------------------------------#
  # Create histogram of restricted activity days by diagnosis of chronic illness
  plotChronicHist <- function(plotData, means) {

    # Create histogram of Dr. visitsgrouped a diagnosis of chronic illness
    hist <- ggplot2::ggplot(plotData,
                   ggplot2::aes(x=Visits, color = Chronic, fill = Chronic)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), position = "identity", alpha = 0.6,
                     binwidth = 5) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = Visits, color = Chronic),
                 linetype = 'dashed') +
      ggplot2::scale_fill_brewer(palette = 'Accent') +
      ggplot2::scale_color_brewer(palette = 'Accent') +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::labs(title = "Dr. Visits",
           x = "Dr. Visits", y = 'Density')

    return(hist)
  }
  chronicHist1 <- plotChronicHist(chronicData, chronicMeans)
  chronicHist2 <- plotChronicHist(chronicData5Plus, chronicMeans5Plus)


  #---------------------------------------------------------------------------#
  #                      Create Interaction Plots                             #
  #---------------------------------------------------------------------------#
  # Plot histogram of restricted activity days by chronic illness and depression
  plotinteractionHist <- function(plotData, means) {

    # Create histogram of Dr. visitsby depression and chronic illness
    hist <- ggplot2::ggplot(plotData,
                   ggplot2::aes(x=Visits,  fill = DepressionChronic)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), position = "identity", alpha = 0.6,
                     binwidth = 5) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = Visits),
                 linetype = 'dashed') +
      ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
      ggplot2::scale_color_brewer(palette = 'Greens') +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::labs(title = paste0(strwrap("Dr. Visits",
                                  width = 80), collapse = '\n'),
           x = "Health-Related Restricted Activity Days", y = 'Density')

    return(hist)
  }
  interactionHist1 <- plotinteractionHist(interactionData, interactionMeans)
  interactionHist2 <- plotinteractionHist(interactionData5Plus, interactionMeans5Plus)

  #---------------------------------------------------------------------------#
  # Create Interaction Violin Plot
  interactionViolin <- ggplot2::ggplot(interactionData,
                           ggplot2::aes(x = DepressionChronic, y = Visits, fill = DepressionChronic)) +
    ggplot2::geom_violin() +
    ggplot2::labs(title = paste0(strwrap("Dr. Visits",
                                width = 40), collapse = '\n'),
         x = 'Depression / Chronic Illness',
         y = 'Dr. Visits') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    stat_summary(fun.y = mean, geom = 'point') +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Create Interaction Box Plot
  interactionBox <- ggplot2::ggplot(interactionData,
                              ggplot2::aes(x = DepressionChronic, y = Visits, fill = DepressionChronic)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = paste0(strwrap("Dr. Visits",
                                width = 40), collapse = '\n'),
         x = 'Depression / Chronic Illness',
         y = 'Dr. Visits') +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = 'Greens', direction = -1) +
    stat_summary(fun.y = mean, geom = 'point') +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          legend.position = 'bottom')

  #---------------------------------------------------------------------------#
  # Create model for interaction plot
  interactionModel <- lm(Visits ~ Depression + Chronic + Depression*Chronic,
                         data = interactionData)
  interactionAnova <- anova(interactionModel)

  #---------------------------------------------------------------------------#
  # Create Mean Doctor Visits by Chronic Illness Bar Plot
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
    ggplot2::labs(title = 'Mean Dr. Visits by Condition',
         y = 'Dr. Visits')

  #---------------------------------------------------------------------------#
  #                           Conduct Tests                                   #
  #---------------------------------------------------------------------------#
  # Conduct Wilcox and effect tests for depression on Dr. Visits
  depressionTest <- wilcox.test(Visits ~ Depression, data = depressionData,
                                conf.int = TRUE)
  yesGroup <- depressionData %>% filter(Depression == 'Yes')
  noGroup <- depressionData %>% filter(Depression == 'No')
  depressionEffect <- ks.test(yesGroup$Visits, noGroup$Visits)

  # Conduct Wilcox and effect tests for chronic illness on Dr. Visits
  chronicTest <- wilcox.test(Visits ~ Chronic, data = chronicData,
                             conf.int = TRUE)
  yesGroup <- chronicData %>% filter(chronic == 'Yes')
  noGroup <- chronicData %>% filter(chronic == 'No')
  chronicEffect <- ks.test(yesGroup$Visits, noGroup$Visits)

  # Conduct Kruskal and effect tests for depression n& chronic illness on Dr. Visits
  interactionTest <- kruskal.test(Visits ~ DepressionChronic, data = interactionData)

  # Partition data by explanatory variable
  group1 <- interactionData %>% filter(Depression == 'Yes' & Chronic == 'Yes') %>% select(Visits)
  group2 <- interactionData %>% filter(Depression == 'No' & Chronic == 'Yes') %>% select(Visits)
  group3 <- interactionData %>% filter(Depression == 'Yes' & Chronic == 'No') %>% select(Visits)
  group4 <- interactionData %>% filter(Depression == 'No' & Chronic == 'No') %>% select(Visits)

  # Conduct pairwise wilcox teste of equal medians
  test1 <- wilcox.test(group1$Visits, group2$Visits, conf.int = TRUE)
  test2 <- wilcox.test(group2$Visits, group3$Visits, conf.int = TRUE)
  test3 <- wilcox.test(group3$Visits, group4$Visits, conf.int = TRUE)


  # Calculate effect sizes for each
  effect1 <- ks.test(group1$Visits, group2$Visits)
  effect2 <- ks.test(group2$Visits, group3$Visits)
  effect3 <- ks.test(group3$Visits, group4$Visits)

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
      depressionHist1 = depressionHist1,
      depressionHist2 = depressionHist2,
      depressionViolin = depressionViolin,
      depressionBox = depressionBox,
      chronicHist1 = chronicHist1,
      chronicHist2 = chronicHist2,
      chronicViolin = chronicViolin,
      chronicBox = chronicBox,
      allChronic = allChronicBar,
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
