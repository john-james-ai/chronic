#==============================================================================#
#                              analyzeRq1.R                                    #
#==============================================================================#
#'
#' analyzeRq1
#'
#' \code{analyzeRq1(brfss)}{Performs analysis of research question 1:
#' Is there an association between education and income and the prevalence
#' of depression? }
#'
#' Function that performs the analysis for research question #1:  Is there an
#' association between education and income and the prevalence of depression?
#' The function contains __ primary sections: (!) create Data, (2), create
#' tables, (3), conduct tests, (4) compute oods ratios, and (5) render
#' plots.
#'
#' @section Create Data:
#' This section filters the preprocessed BRFSS data to include valid values
#' of income, education and a depression diagnosis. An incoime/education
#' interaction variable is created. Finally depression diagnosis data is
#' rendered by income, education and the income/education interaction
#' variable. The variables are as follows:
#' \itemize{
#'  \item{rq1Data}{Subset of BRFSS data including valid values for Income, Education, and Depression Diagnosis}
#'  \item{rq2Data$IncomeEducation}{Income/Education interaction variable}
#'  \item{rq1IncomeData}{Income and depression diagnosis data from preprocessed BRFSS data}
#'  \item{rq1EducationData}{Education and depression diagnosis data from preprocessed BRFSS data}
#'  \item{rq1InteractionData}{Income / Education interaction and depression diagnosis data from preprocessed BRFSS data}
#' }
#'
#' @section Create Tables:
#' This section converts the income, education and depression diagnosis
#' data to frequency, proportion, interaction, joint frequency, joint
#' proportion, conditional frequency and conditional proportion tables,
#' The variables are:
#' \itemize{
#'  \item{rq1Table}{rq1Data in table format}
#'  \item{rq1IncomeTable}{rq1IncomeData in table format}
#'  \item{rq1EducationTable}{rq1EducationData in table format}
#'  \item{rq1InteractionTable}{rq1InteractionData in table format}
#'  \item{rq1IncomeFreqTable}{Frequencies of depression diagnoses by income level}
#'  \item{rq1IncomePropTable}{Proportions of depression diagnoses by income level}
#'  \item{rq1EducationFreqTable}{Frequencies of depression diagnoses by education level}
#'  \item{rq1EducationPropTable}{Proportions of depression diagnoses by education level}
#'  \item{rq1JointFreq}{Frequencies of depression diagnoses by income and education level}
#'  \item{rq1JointProp}{Proportions of depression diagnoses by income and education level}
#'  \item{rq1CondFreq}{Frequencies of depression diagnoses by income, given education level education level}
#'  \item{rq1JointFreqF}{Joint frequency table in data frame format.}
#'  \item{rq1IncomeFreqF}{Income frequency table in data frame format.}
#'  \item{rq1EducationFreqF}{Education frequency table in data frame format.}
#'  \item{rq1InteractionFreqF}{Income/education interaction frequency table in data frame format.}
#'  \item{rq1JointPropF}{Joint proportion table in data frame format.}
#'  \item{rq1IncomePropF}{Income proportion table in data frame format.}
#'  \item{rq1EducationPropF}{Education proportion table in data frame format.}
#'  \item{rq1InteractionPropF}{Income/education interaction proportion table in data frame format.}#'
#' }
#'
#' @section Conduct Tests:
#' This section performs the statistical tests that reveal the associations
#' between depression and income and education.  First, chi-squaared tests are
#' conducted to ascertain marginal and joint independence of income and education
#' and depression diagnoses.  Cramer's V tests quantified the strength of any
#' association. The Cochran-Mantel-Haenszel test was administered to measure
#' three-way independence between depression diagnoses and income and education.
#' The variables are as follows:
#' \itemize{
#'  \item{rq1IncomeInd}{Chi-squared tests of marginal indpendence of income on depression}
#'  \item{rq1EducationInd}{Chi-squared tests of marginal indpendence of education on depression}
#'  \item{rq1JointInd}{Chi-squared tests of joint indpendence of income and education on depression}
#'  \item{rq1IncomeIndV}{Cramer's V test of the strength of the association of income and depression}
#'  \item{rq1EducationIndV}{Cramer's V test of the strength of the association of education and depression}
#'  \item{rq1JointIndV}{Cramer's V test of the strength of the joint association of education and income on depression}
#'  \item{rq1CondInd}{Cochran-Mantel-Haenszel test of conditional independence}
#'  \item{rq1CondIndDf}{Cochran-Mantel-Haenszel test of conditional independence in data frame format}
#'  \item{rq1Anova}{Tests to determine which indpendent variable has greatest effect on the response, depression diagnoses}
#' }
#'
#' @section Odds Ratios:
#' This section computes the odds ratios for marginal, conditional
#' and joint associations. The variables are as follows:
#' \itemize{
#'  \item{rq1IncomeOdds}{The marginal odds of a depression diagnosis at each level of income}
#'  \item{rq1IncomeOdds}{The marginal odds of a depression diagnosis at each level of education}
#'  \item{rq1ConditionalOdds}{The odds of a depression diagnosis at each level of income, given education}
#'  \item{rq1InteractionOdds}{The odds of a depression diagnosis at each level of income and education}
#' }
#'
#' @section Create Plots:
#' This section renders the plots for this analysis. Stacked bar plots showing
#' marginal frequency and proportions are rendered for both income and education.
#' Conditional proportion plots are rendered for education and income interaction,
#' holding education constant.  The variables are:
#' \itemize{
#'  \item{rq1IncomeFreqBar}{Stacked bar plot of the frequencies of depression diagnoses at all income levels}
#'  \item{rq1IncomePropBar}{Stacked bar plot of the proportions of depression diagnoses at all income levels}
#'  \item{rq1EducationFreqBar}{Stacked bar plot of the frequencies of depression diagnoses at all education levels}
#'  \item{rq1EducationPropBar}{Stacked bar plot of the proportions of depression diagnoses at all education levels}
#'  \item{rq1ConditionalPlots}{Stacked bar plot of the proportions of depression diagnoses at all income/education levels, holding education constant}
#' }
#'
#' @param brfss Data frame of preprocessed 2013 Behavioral Risk Factor
#' Surveillance System (BRFSS) telephone survey.

#' @return A nested list containing marginal, conditional, and joint
#' association frequencies and proportions, plots, statistical tests
#' and odds ratios. The return variables are as follows:
#' \itemize{
#'  \itemize{
#'   \item{freq}{Depression diagnosis frequency table}
#'   \item{jointFreq}{Income and education joint association frequency table}
#'   \item{jointProp}{Income and education joint association proportion table}
#'   \item{condFreq}{Income and education conditional association frequency table}
#'   \item{condProp}{Income and education conditional association proportion table}
#'   \item{incomeTable}{Depression diagnoses by income table}
#'   \item{incomeFreq}{Frequency of depression diagnoses by level of income table}
#'   \item{incomeProp}{Proportion of depression diagnoses by level of income table}
#'   \item{educationTable}{Depression diagnoses by education table}
#'   \item{educationFreq}{Frequency of depression diagnoses by level of education table}
#'   \item{educationProp}{Proportion of depression diagnoses by level of education table}
#'   \item{interactionTable}{Depression diagnoses by income/education interaction table}
#'   \item{interactionFreq}{Frequency of depression diagnoses by income/education interaction table}
#'   \item{interactionProp}{Proportion of depression diagnoses by income/education interaction table}#'
#'  }
#'  \itemize{
#'   \item{freqDf}{Frequency of depression diagnoses by income/education joint association in data frame format}
#'   \item{propDf}{Proportion of depression diagnoses by income/education joint association in data frame format}
#'   \item{incomeFreqDf}{Frequency of depression diagnoses at each level of income in data frame format}
#'   \item{incomePropDf}{Proportion of depression diagnoses at each level of income in data frame format}
#'   \item{educationFreqDf}{Frequency of depression diagnoses at each level of education in data frame format}
#'   \item{educationPropDf}{Proportion of depression diagnoses at each level of education in data frame format}
#'   \item{interactionFreqDf}{Frequency of depression diagnoses at each level of income/education interaction in data frame format}
#'   \item{interactionPropDf}{Proportion of depression diagnoses at each level of income/education interaction in data frame format}
#'  }
#'  \itemize{
#'   \item{incomeFreq}{Frequency of depression diagnoses by level of income plot}
#'   \item{incomeProp}{Proportion of depression diagnoses by level of income plot}
#'   \item{educationFreq}{Frequency of depression diagnoses by level of education plot}
#'   \item{educationProp}{Proportion of depression diagnoses by level of education plot}
#'   \item{conditionalProp}{Proportion of depression diagnoses by level of income/education interaction, holding education constant}
#'  }
#'  \itemize{
#'   \item{incomeInd}{Test of marginal independence of income on depression}
#'   \item{educationInd}{Test of marginal independence of education on depression}
#'   \item{incomeIndV}{Cramer's V measure of strength of association of income and depression}
#'   \item{educationIndV}{Cramer's V measure of strength of association of education and depression}
#'   \item{jointInd}{Test of joint independence of income and education on depression}
#'   \item{jointIndV}{Cramer's V measure of strength of the joint association of education and income on depression}
#'   \item{condInd}{Test of conditional independence of income on depression, given education}
#'   \item{anova}{Test of relative effects of income and education on depression}
#'  }
#'  \itemize{
#'   \item{incomeOdds}{Odds of a depression diagnosis at each level of income}
#'   \item{educationOdds}{Odds of a depression diagnosis at each level of education}
#'   \item{conditionalOdds}{Odds of a depression diagnosis at each level of income, given education}
#'   \item{interactionOdds}{Odds of a depression diagnosis at each level of income and education}
#'  }
#'
#' }
#' @family Research Question Analysis Functions
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
analyzeRq1 <- function(brfss) {

  #---------------------------------------------------------------------------#
  #                           Create Data                                     #
  #---------------------------------------------------------------------------#

  # Get data
  rq1Data <- brfss %>%
    filter(!is.na(Income) & !is.na(Education) & !is.na(Depression)) %>%
    select(Income, Education,  Depression)

  # Format interaction variable
  rq1Data$IncomeEducation <- interaction(rq1Data$Income, rq1Data$Education)

  rq1IncomeData <- rq1Data %>% select(Income, Depression)
  rq1EducationData <- rq1Data %>% select(Education, Depression)
  rq1InteractionData <- rq1Data %>% select(IncomeEducation, Depression)

  #---------------------------------------------------------------------------#
  #                           Create Tables                                   #
  #---------------------------------------------------------------------------#
  # Render basic tables
  rq1Table <- with(rq1Data, table(Income, Education, Depression))
  rq1IncomeTable <- with(rq1Data, table(Income, Depression))
  rq1EducationTable <- with(rq1Data, table(Education, Depression))
  rq1InteractionTable <- with(rq1Data, table(IncomeEducation, Depression))

  # Create 2-Way Marginal Contingency Tables on Income
  rq1IncomeFreqTable <- ftable(addmargins(rq1IncomeTable))
  rq1IncomePropTable <- ftable(prop.table(rq1IncomeTable, 1))

  # Create 2-Way Marginal Contingency Tables on Education
  rq1EducationFreqTable <- ftable(addmargins(rq1EducationTable))
  rq1EducationPropTable <- ftable(prop.table(rq1EducationTable, 1))

  # Create 2-Way Joint Contingency Table
  rq1InteractionFreqTable <- ftable((addmargins(rq1InteractionTable)))
  rq1InteractionPropTable <- ftable((prop.table(rq1InteractionTable, 1)))

  # Create 3-Way Joint Frequency and Proportion Tables
  rq1JointFreq <- ftable(rq1Table)
  rq1JointProp <- ftable(prop.table(rq1Table, 1))

  # Create conditional tables
  rq1CondFreq <- ftable(Depression ~ Income + Education, data = rq1JointFreq)
  rq1CondProp <- ftable(Depression ~ Income + Education, data = rq1JointProp)


  # Create frequency data frames
  rq1JointFreqDf <- as.data.frame(rq1JointFreq)
  rq1IncomeFreqDf <- as.data.frame(rq1IncomeTable)
  rq1EducationFreqDf <- as.data.frame(rq1EducationTable)
  rq1InteractionFreqDf <- as.data.frame(rq1InteractionTable)


  # Create proportion data frames
  rq1JointPropDf <- as.data.frame(rq1JointProp)
  rq1IncomePropDf <- as.data.frame(rq1IncomePropTable)
  rq1EducationPropDf <- as.data.frame(rq1EducationPropTable)
  rq1InteractionPropDf <- as.data.frame(rq1InteractionPropTable)


  #---------------------------------------------------------------------------#
  #                           Conduct Tests                                   #
  #---------------------------------------------------------------------------#
  # Test for Marginal Independence
  rq1IncomeInd <- chisq.test(rq1IncomeFreqTable)
  rq1EducationInd <- chisq.test(rq1EducationFreqTable)

  # Test for strength of marginal association
  rq1IncomeIndV <- vcd::assocstats(rq1IncomeFreqTable)
  rq1EducationIndV <- vcd::assocstats(rq1EducationFreqTable)

  # Test for Joint Independence
  rq1JointInd <- chisq.test(rq1InteractionFreqTable)
  rq1JointIndV <- vcd::assocstats(rq1InteractionFreqTable)

  # Test of 3-Way conditional independence and format results
  rq1CondFreqXt <- xtabs(Freq ~ Depression +  Income + Education, data = rq1JointFreq)
  rq1CondInd <- vcdExtra::CMHtest(rq1CondFreqXt)
  educationLevels <- c('Did not graduate high school',
                       'Graduated high school',
                       'Attended college or technical school',
                       'Graduated from college or technical school')
  ha <- c('Nonzero correlation',
          'Row mean scores differ',
          'Col mean scores differ',
          'General association')
  rq1CondIndDf <- lapply(seq_along(rq1CondInd), function(x) {
    df <- data.frame(Parameter = dimnames(rq1CondInd[[x]]$table)[[1]],
                     AltHypothesis = ha,
                     Chisq = round(rq1CondInd[[x]]$table[,1], 0),
                     Df = round(rq1CondInd[[x]]$table[,2], 0),
                     Prob = round(rq1CondInd[[x]]$table[,3], 4),
                     row.names = NULL)
    stratum <- educationLevels[x]
    condInd <- list(
      df = df,
      stratum = stratum
    )
    condInd
  })

  # Conduct test to determine which variable has the greatest effect on response
  rq1Model <- glm(Depression ~ Income + Education + Income * Education,
                  data = rq1Data, family = 'binomial')
  rq1Anova <- anova(rq1Model, test = 'Chisq')

  #---------------------------------------------------------------------------#
  #                         Create Odds Ratios                                #
  #---------------------------------------------------------------------------#
  # Computation of Odds of Depression by Income Level
  rq1IncomeOdds <- vcd::oddsratio(rq1IncomeTable, stratum = NULL, log = FALSE)
  dims <- names(rq1IncomeOdds$coefficients)
  odds <- exp(as.numeric(rq1IncomeOdds$coefficients))
  rq1IncomeOdds <- data.frame(Dimension = dims, Odds = odds)

  # Computation of Odds of Depression by Education
  rq1EducationOdds <- vcd::oddsratio(rq1EducationTable, stratum = NULL, log = FALSE)
  dims <- names(rq1EducationOdds$coefficients)
  odds <- exp(as.numeric(rq1EducationOdds$coefficients))
  rq1EducationOdds <- data.frame(Dimension = dims, Odds = odds)

  # Computation of Odds of Income given Education
  rq1ConditionalOdds <- vcd::loddsratio(Freq ~ Income | Education, data = rq1Data)
  dims <- names(rq1ConditionalOdds$coefficients)
  odds <- exp(rq1ConditionalOdds$coefficients)
  rq1ConditionalOdds <- data.frame(Dimension = dims, Odds = odds)

  # Computation of Odds of Depression by Education and Income
  rq1InteractionOdds <- vcd::oddsratio(rq1InteractionTable, stratum = NULL, log = FALSE)
  dims <- names(rq1InteractionOdds$coefficients)
  odds <- exp(as.numeric(rq1InteractionOdds$coefficients))
  rq1InteractionOdds <- data.frame(Dimension = dims, Odds = odds)
  rq1InteractionOdds <-
    within(rq1InteractionOdds, level <-
             data.frame(do.call('rbind',
                                strsplit(as.character(rq1InteractionOdds$Dimension),
                                         '.', fixed = TRUE))))
  rq1InteractionOdds <-
    within(rq1InteractionOdds, level2 <-
             data.frame(do.call('rbind',
                                strsplit(as.character(rq1InteractionOdds$level$X2),
                                         ':', fixed = TRUE))))
  rq1InteractionOdds <- data.frame(Education = rq1InteractionOdds$level$X3,
                                   Income = paste(rq1InteractionOdds$level$X1,
                                                  '/', rq1InteractionOdds$level2$X2,
                                                  ':Yes/No'),
                                   Odds = round(rq1InteractionOdds$Odds, 2))


  # Computation of conditional odds rations of Depression by Income / Education
  rq1ConditionalOdds <- vcd::oddsratio(rq1CondFreqXt, stratum = NULL, log = FALSE)
  educ <- rq1ConditionalOdds$dimnames$Education
  inc <- rq1ConditionalOdds$dimnames$Income
  rq1ConditionalOddsDf <- lapply(seq_along(educ), function(x) {
    education <- educ[x]
    df <- data.frame(Income = inc,
                     Odds = exp(rq1ConditionalOdds$coefficients[(((x-1)*4)+1):(x*4)]),
                     row.names = NULL)
    odds <- list(
      stratum = education,
      df = df
    )
    odds
  })

  #---------------------------------------------------------------------------#
  #                           Create Plots                                    #
  #---------------------------------------------------------------------------#
  # Add position variables to frequency data frames for plotting
  rq1EducationFreqDf <- rq1EducationFreqDf %>% arrange(Education, desc(Depression)) %>%
    group_by(Education) %>% mutate(cumFreq = cumsum(Freq), pos = cumFreq-0.5*Freq)
  rq1IncomeFreqDf <- rq1IncomeFreqDf %>% arrange(Income, desc(Depression)) %>%
    group_by(Income) %>% mutate(cumFreq = cumsum(Freq), pos = cumFreq-0.5*Freq)
  rq1InteractionFreqDf <- rq1InteractionFreqDf %>% arrange(IncomeEducation, desc(Depression)) %>%
    group_by(IncomeEducation) %>% mutate(cumFreq = cumsum(Freq), pos = cumFreq- 0.5*Freq)

  # Add position variables to proportion data frames for plotting
  rq1EducationPropDf <- rq1EducationPropDf %>% arrange(Education, desc(Depression)) %>%
    group_by(Education) %>% mutate(cumFreq = cumsum(Freq), pos = cumFreq-0.5*Freq)
  rq1IncomePropDf <- rq1IncomePropDf %>% arrange(Income, desc(Depression)) %>%
    group_by(Income) %>% mutate(cumFreq = cumsum(Freq), pos = cumFreq-0.5*Freq)
  rq1InteractionPropDf <- rq1InteractionPropDf %>% arrange(IncomeEducation, desc(Depression)) %>%
    group_by(IncomeEducation) %>% mutate(cumFreq = cumsum(Freq), pos = cumFreq- 0.5*Freq)

  #---------------------------------------------------------------------------#
  # Create Marginal Frequency Stacked Bar Plot of Depression by Income Category
  rq1IncomeFreqBar <- ggplot2::ggplot(rq1IncomeData, ggplot2::aes(x=Income)) +
    ggplot2::geom_bar(ggplot2::aes(fill = Depression)) + ggplot2::theme_minimal() +
    ggplot2::geom_text(data = rq1IncomeFreqDf, ggplot2::aes(x = Income, y = pos, label = Freq),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme(text=ggplot2::element_text(family="Open Sans"), legend.position = "bottom") +
    ggplot2::scale_fill_brewer(palette="Greens") +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::labs(x = "Income",  y = 'Count') +
    ggplot2::ggtitle('Frequency Distribution')


  # Create Marginal Proportion Stacked Normalized Bar Plot of Depression by Income
  rq1IncomePropBar <- ggplot2::ggplot(rq1IncomeData, ggplot2::aes(x=Income)) +
    ggplot2::geom_bar(ggplot2::aes(fill = Depression), position = 'fill') + ggplot2::theme_minimal() +
    ggplot2::geom_text(data = rq1IncomePropDf,
              ggplot2::aes(x = Income, y = pos,label = round(Freq, 2)),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme(text=ggplot2::element_text(family="Open Sans"), legend.position = "bottom") +
    ggplot2::scale_fill_brewer(palette="Greens") +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::labs(x = "Income",  y = 'Proportion') +
    ggplot2::ggtitle('Proportional Distribution')

  #---------------------------------------------------------------------------#
  # Create Marginal Frequency Stacked Bar Plot of Depression by Education Category
  rq1EducationFreqBar <- ggplot2::ggplot(rq1EducationData, ggplot2::aes(x=Education)) +
    ggplot2::geom_bar(ggplot2::aes(fill = Depression)) + ggplot2::theme_minimal() +
    ggplot2::geom_text(data = rq1EducationFreqDf, ggplot2::aes(x = Education, y = pos, label = Freq),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme(text=ggplot2::element_text(family="Open Sans"), legend.position = "bottom") +
    ggplot2::scale_fill_brewer(palette="Greens") +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::labs(x = "Education",  y = 'Count') +
    ggplot2::ggtitle('Frequency Distribution')

  # Create Marginal Proportion Stacked Normalized Bar Plot of Depression by Education
  rq1EducationPropBar <- ggplot2::ggplot(rq1EducationData, ggplot2::aes(x=Education)) +
    ggplot2::geom_bar(ggplot2::aes(fill = Depression), position = 'fill') + ggplot2::theme_minimal() +
    ggplot2::geom_text(data = rq1EducationPropDf,
              ggplot2::aes(x = Education, y = pos,label = round(Freq, 2)),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme(text=ggplot2::element_text(family="Open Sans"), legend.position = "bottom") +
    ggplot2::scale_fill_brewer(palette="Greens") +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::labs(x = "Education",  y = 'Proportion') +
    ggplot2::ggtitle('Proportional Distribution')

  #---------------------------------------------------------------------------#
  # Plot conditional association function
  plotConditionalProp <- function(plotData, textData, education ) {
    bar <- ggplot2::ggplot(plotData, ggplot2::aes(x=IncomeEducation)) +
      ggplot2::geom_bar(ggplot2::aes(fill = Depression), position = 'fill') + ggplot2::theme_minimal() +
      ggplot2::geom_text(data = textData,
                ggplot2::aes(x = IncomeEducation, y = pos,label = round(Freq, 2)),
                colour="black", family="Tahoma", size=4) +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans"), legend.position = "bottom",
            plot.title = ggplot2::element_text(size = 10, family="Open Sans"),
            axis.text.x = ggplot2::element_text(size = 8),
            axis.title.x = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Greens") +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 4)) +
      ggplot2::labs(title = education,
           y = 'Proportion')
    return(bar)
  }

  # Create conditional association plots
  education <- unique(rq1Data$Education)

  rq1ConditionalPlots <- lapply(seq_along(education), function(x) {
    plotConditionalProp(rq1InteractionData %>%
                          filter(grepl(education[x], IncomeEducation )),
                        rq1InteractionPropDf %>%
                          filter(grepl(education[x], IncomeEducation )),
                        education[x])
  })


  #---------------------------------------------------------------------------#
  #                     Format and Return Results                             #
  #---------------------------------------------------------------------------#
  analysis <- list(
    tables = list(
      freq = rq1Table,
      jointFreq = rq1JointFreq,
      jointProp = rq1JointProp,
      condFreq = rq1CondFreq,
      condProp = rq1CondProp,
      incomeTable = rq1IncomeTable,
      incomeFreq = rq1IncomeFreqTable,
      incomeProp = rq1IncomePropTable,
      educationTable = rq1EducationTable,
      educationFreq = rq1EducationFreqTable,
      educationProp = rq1EducationPropTable,
      interactionTable = rq1InteractionTable,
      interactionFreq = rq1InteractionFreqTable,
      interactionProp = rq1InteractionPropTable
    ),
    dataFrames = list(
      freqDf = rq1JointFreqDf,
      propDf = rq1JointPropDf,
      incomeFreqDf = rq1IncomeFreqDf,
      educationFreqDf = rq1EducationFreqDf,
      interactionFreqDf = rq1InteractionFreqDf,
      incomePropDf = rq1IncomePropDf,
      educationPropDf = rq1EducationPropDf,
      interactionPropDf =  rq1InteractionPropDf
    ),
    plots = list(
      incomeFreq = rq1IncomeFreqBar,
      incomeProp = rq1IncomePropBar,
      educationFreq = rq1EducationFreqBar,
      educationProp = rq1EducationPropBar,
      conditionalProp = rq1ConditionalPlots
    ),
    tests = list(
      incomeInd = rq1IncomeInd,
      educationInd = rq1EducationInd,
      incomeIndV = rq1IncomeIndV,
      educationIndV = rq1EducationIndV,
      jointInd = rq1JointInd,
      jointIndV = rq1JointIndV,
      condInd = rq1CondIndDf,
      anova = rq1Anova
    ),
    analysis = list(
      incomeOdds = rq1IncomeOdds,
      educationOdds = rq1EducationOdds,
      conditionalOdds = rq1ConditionalOddsDf,
      interactionOdds = rq1InteractionOdds
    )
  )
  return(analysis)

}
## ---- end
