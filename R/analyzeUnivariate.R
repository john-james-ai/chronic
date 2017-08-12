#==============================================================================#
#                              analyzeUnivariate                               #
#==============================================================================#
#' analyzeUnivariate
#'
#'  \code{analyzeUnivariate}
#'
#' This function performs the univariate analysis of categorical and quantitative
#' variables.
#'
#' @param brfss Data frame of preprocessed 2013 Behavioral Risk Factor Surveillance System (BRFSS) data
#' @return univariate List of analyses for categorical and quantitative variables
#' @author John James, \email{jjames@@dataference.com}
#' @export
analyzeUnivariate <- function(brfss) {

  analyzeDepression <- function(brfss) {

    # Frequency Distribution Table
    depression <- brfss %>% group_by(Depression) %>% dplyr::summarize(N=n()) %>%
      mutate(Pct = round(N / sum(N) * 100, 1), Cumulative = round(cumsum(Pct), 1))

    # Summarize Variable
    summary <- data.frame(Variable = 'Depression', BRFSS = 'addepev2',
                          Levels = count(unique(depression %>%
                                                  filter(!is.na(Depression)) %>%
                                                  select(Depression)))$n,
                          Responses = sum(depression$N),
                          Valid = sum(depression %>% filter(!is.na(Depression)) %>%
                                        select(N)),
                          NAs = (depression %>% filter(is.na(Depression)) %>%
                            select(N))$N,
                          Rate = round(sum(depression %>%
                                             filter(!is.na(Depression)) %>%
                                             select(N)) / sum(depression$N) * 100, 2))

    # Add total line
    depression <- rbind(depression,
                        data.frame(Depression = 'Total', N = sum(depression$N),
                                   Pct = sum(depression$Pct), Cumulative = ' '))

    # Create table grob
    depressionGrob <- gridExtra::tableGrob(format(depression, big.mark = ",",
                                       digits = 0),
                                rows = NULL,
                                theme = gridExtra::ttheme_minimal(base_size = 10,
                                                       padding = grid::unit(c(4, 2), "mm")))

    # Estimate difference in proportions from hypothesized population proportion
    zTest <- prop.test(x = depression[1,2]$N, n = sum(depression[1:2,2]$N), p = .182)

    # Bar Plot
    depressionPlot <- ggplot2::ggplot(data = depression %>%
                               filter(Depression %in% c('Yes', 'No')),
                             ggplot2::aes(x =Depression, y=N, fill = Depression)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens')

    analysis <- list(
      table = depression,
      grob = depressionGrob,
      plot = depressionPlot,
      zTest = zTest,
      summary = summary
    )

    return(analysis)
  }
  ## ---- end

  ## ---- analyze_chronic
  analyzeChronic <- function(brfss) {

    # Table
    chronic <- data.table::as.data.table(brfss %>% group_by(Chronic) %>% dplyr::summarize(N=as.numeric(n())) %>%
      mutate(Pct = round(N / sum(N) * 100, 1), Cumulative = round(cumsum(Pct), 1)))

    # Summarize Variable
    levels <- dplyr::count(unique(chronic %>%
                             filter(!is.na(Chronic)) %>%
                             select(Chronic)))$n
    responses <- sum(chronic$N)
    valid <- sum(subset(chronic, !is.na(Chronic), select=c(N))$N)
    rate <- round(valid/responses * 100,2)
    summary <- data.frame(Variable = "Chronic", BRFSS = '<derived>',
                          Levels = levels,
                          Responses = responses,
                          Valid = valid,
                          NAs = 0,
                          Rate = rate)

    # Add Total Line
    chronic <- rbind(chronic, data.frame(Chronic = 'Total',
                                         N = sum(chronic$N),
                                         Pct = sum(chronic$Pct), Cumulative = ' '))

    # Create table grob
    chronicGrob <- gridExtra::tableGrob(format(chronic, big.mark = ",",
                                       digits = 0),
                                rows = NULL,
                                theme = gridExtra::ttheme_minimal(base_size = 10,
                                                       padding = grid::unit(c(4, 2), "mm")))

    # Estimate difference in proportions from hypothesized population proportion
    zTest <- prop.test(x = chronic[1,2]$N, n = sum(chronic[1:2,2]$N), p = .6)

    # Bar Plot
    chronicPlot <- ggplot2::ggplot(chronic[-3,], ggplot2::aes(x =Chronic, y=N, fill = Chronic)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = "none") +
      ggplot2::scale_fill_brewer(palette="Greens") +
      ggplot2::ggtitle('Frequency Distribution of Chronic Disease Diagnoses')

    analysis <- list(
      table = chronic,
      grob = chronicGrob,
      plot = chronicPlot,
      zTest = zTest,
      summary = summary
    )

    return(analysis)
  }
  ## ---- end

  ## ---- analyze_income
  analyzeIncome <- function(brfss) {

    # Frequency Distribution Table
    income <- brfss %>% group_by(Income) %>% dplyr::summarize(N=n()) %>%
      mutate(Pct = round(N / sum(N) * 100, 1), Cumulative = round(cumsum(Pct), 1))

    # Summarize variable
    summary <- data.frame(Variable = 'Income', BRFSS = 'X_incomg',
                          Levels = count(unique(income %>%
                                                  filter(!is.na(Income)) %>%
                                                  select(Income)))$n,
                          Responses = sum(income$N),
                          Valid = sum(income %>% filter(!is.na(Income)) %>%
                                        select(N)),
                          NAs = (income %>% filter(is.na(Income)) %>%
                                   select(N))$N,
                          Rate = round(sum(income %>%
                                             filter(!is.na(Income)) %>%
                                             select(N)) / sum(income$N) * 100, 2))

    # Add total line
    income <- rbind(income,
                    data.frame(Income = 'Total', N = sum(income$N),
                               Pct = sum(income$Pct), Cumulative = ' '))

    # Frequency Distribution Table Grob
    incomeGrob <- gridExtra::tableGrob(format(income, big.mark = ",",
                                   digits = 0), rows = NULL,
                            theme = gridExtra::ttheme_minimal(base_size = 10,
                                                   padding = grid::unit(c(4, 2), "mm")))

    # Bar Plot
    incomePlot <- ggplot2::ggplot(data = income %>%
                           filter(Income != 'NA', Income != 'Total'),
                         ggplot2::aes(x =Income, y=N, fill = Income)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = "none") +
      ggplot2::scale_fill_brewer(palette="Greens") +
      ggplot2::ggtitle('Frequency Distribution of Income')

    analysis <- list(
      table = income,
      grob = incomeGrob,
      plot = incomePlot,
      summary = summary
    )
    return(analysis)
  }


  ## ---- analyze_education
  analyzeEducation <- function(brfss) {

    # Frequency Distribution Table
    education <- brfss %>% group_by(Education) %>% dplyr::summarize(N=n()) %>%
      mutate(Pct = round(N / sum(N) * 100, 1), Cumulative = round(cumsum(Pct), 1))

    # Summarize Variable
    summary <- data.frame(Variable = 'Education', BRFSS = 'X_educag',
                          Levels = count(unique(education %>%
                                                  filter(!is.na(Education)) %>%
                                                  select(Education)))$n,
                          Responses = sum(education$N),
                          Valid = sum(education %>% filter(!is.na(Education)) %>%
                                        select(N)),
                          NAs = (education %>% filter(is.na(Education)) %>%
                                   select(N))$N,
                          Rate = round(sum(education %>%
                                             filter(!is.na(Education)) %>%
                                             select(N)) / sum(education$N) * 100, 2))
    # Add Total Line
    education <- rbind(education,
                       data.frame(Education = 'Total', N = sum(education$N),
                                  Pct = sum(education$Pct), Cumulative = ' '))

    # Frequency Distribution Table Grob
    educationGrob <- gridExtra::tableGrob(format(education, big.mark = ",",
                                      digits = 0),
                               rows = NULL,
                               theme = gridExtra::ttheme_minimal(base_size = 10,
                                                      padding = grid::unit(c(4, 2), "mm")))

    # Bar Plot
    educationPlot <- ggplot2::ggplot(data = education %>%
                              filter(Education != 'NA', Education != 'Total'),
                            ggplot2::aes(x =Education, y=N, fill = Education)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = "none") +
      ggplot2::scale_fill_brewer(palette="Greens") +
      ggplot2::ggtitle('Frequency Distribution of Education')

    analysis <- list(
      table = education,
      grob = educationGrob,
      plot = educationPlot,
      summary = summary
    )
    return(analysis)

  }

  ## ---- end


  ## ---- analyze_bad_days_range
  analyzeSickDaysRange <- function(brfss) {

    # Frequency Distribution Table
    days <- brfss %>% group_by(SickDaysRange) %>% dplyr::summarize(N=n()) %>%
      mutate(Pct = round(N / sum(N) * 100, 2), Cumulative = round(cumsum(Pct), 0))

    # Summarize Variable
    summary <- data.frame(Variable = 'Sick Days Categories', BRFSS = 'NA',
                          Levels = count(unique(days %>%
                                                  filter(!is.na(SickDaysRange)) %>%
                                                  select(SickDaysRange)))$n,
                          Responses = sum(days$N),
                          Valid = sum(days %>% filter(!is.na(SickDaysRange)) %>%
                                        select(N)),
                          NAs = (days %>% filter(is.na(SickDaysRange)) %>%
                                   select(N))$N,
                          Rate = round(sum(days %>%
                                             filter(!is.na(SickDaysRange)) %>%
                                             select(N)) / sum(days$N) * 100, 2))
    # Add Total Line
    days <- rbind(days,
                       data.frame(SickDaysRange = 'Total', N = sum(days$N),
                                  Pct = sum(days$Pct), Cumulative = ' '))

    # Frequency Distribution Table Grob
    daysGrob <- gridExtra::tableGrob(format(days, big.mark = ",",
                                      digits = 0),
                               rows = NULL,
                               theme = gridExtra::ttheme_minimal(base_size = 10,
                                                      padding = grid::unit(c(4, 2), "mm")))

    # Bar Plot
    daysPlot <- ggplot2::ggplot(days %>% filter(SickDaysRange != 'Total'),
                       ggplot2::aes(x =SickDaysRange, y=N, fill = SickDaysRange)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal() +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = "none") +
      ggplot2::scale_fill_brewer(palette="Greens") +
      ggplot2::ggtitle('Sick Days') +
      ggplot2::annotation_custom(daysGrob, xmin = 3,
                                  xmax = 6, ymin = 50000, ymax = 150000)

    analysis <- list(
      table = days,
      grob = daysGrob,
      plot = daysPlot,
      summary = summary
    )
    return(analysis)

  }

  ## ---- end


  ## ---- analyze_bad_days
  analyzeSickDays <- function(brfss) {

    # Extracted valid values
    sickDays <- data.frame(SickDays = brfss$SickDays[brfss$SickDays < 31
                                          & !is.na(brfss$SickDays)])
    sickDays2 <- sickDays %>% filter(SickDays > 0)

    # Obtain Skewness, Kurtosis and Confidence Interval
    skew <- psych::describe(sickDays$SickDays)
    ci <- pastecs::stat.desc(sickDays$SickDays)

    # Descriptive Statistics
    getDescriptiveStats <- function(brfss, theData) {
      theSummary <- theData %>%
        dplyr::summarise(Variable = 'SickDays', BRFSS = 'poorhlth',
                  N = length(brfss$SickDays),
                  Valid = length(SickDays),
                  `Response Rate` = round(length(SickDays) /
                                            length(brfss$SickDays) * 100, 2),
                  Min = min(SickDays),
                  Lower = quantile(SickDays, .25),
                  Median = median(SickDays),
                  Mean = round(mean(SickDays)),
                  Mode = getMode(as.vector(SickDays)),
                  CI = as.numeric(round(ci[11], 3)),
                  Upper = quantile(SickDays, .75),
                  Max = max(SickDays),
                  SD = round(sd(SickDays)),
                  SE = round(sd(SickDays)/sqrt(length(SickDays)), 2),
                  Skew = round(skew$skew, 2),
                  Kurtosis = round(skew$kurtosis, 2))
      return(theSummary)
    }
    wideSummary1 <- getDescriptiveStats(brfss, sickDays)
    wideSummary2 <- getDescriptiveStats(brfss, sickDays2)

    # Histogram
    plotHist <- function(sickDays, average) {
      hist <- ggplot2::ggplot(data = sickDays, ggplot2::aes(x =SickDays, fill=..x..)) +
        ggplot2::geom_histogram(binwidth = 1) +
        ggplot2::scale_fill_gradient("Sick Days", low = "seagreen4", high = "royalblue4") +
        ggplot2::theme_minimal() +
        ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = average), color = 'royalblue4',
                   linetype = 'solid', size = .5) +
        ggplot2::labs(x = "Sick Days", y = 'Count') +
        ggplot2::ggtitle('Frequency Distribution of Sick Days in Previous 30 Days')
      return(hist)
    }
    hist1 <- plotHist(sickDays, mean(sickDays$SickDays))
    hist2 <- plotHist(sickDays2, mean(sickDays2$SickDays))

    # Boxplot
    box <- ggplot2::ggplot(data = sickDays, ggplot2::aes(x = 'SickDays', y=SickDays)) +
      ggplot2::geom_boxplot(fill = 'seagreen4') +
      ggplot2::ggtitle("Boxplot of Sick Days in Previous 30 Days") +
      ggplot2::theme_minimal() +
      ggplot2::geom_point(data = data.frame(SickDays = mean(sickDays$SickDays)),
                 ggplot2::aes(x ='SickDays', y=SickDays)) +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::labs(x = 'Sick Days' ,
           y = 'Distribution of Sick Days') +
      ggplot2::theme(axis.text.y= ggplot2::element_blank(),
            legend.position = 'none')

    analysis <- list(
      stats1 = wideSummary1,
      stats2 = wideSummary2,
      hist1 = hist1,
      hist2 = hist2,
      box = box
    )

    return(analysis)

  }
  ## ---- end


  ## ---- analyze_visits
  analyzeVisits <- function(brfss) {

    # Extract valid data for plotting
    visits <- brfss %>% filter(!is.na(Visits))

    # Obtain Skewness, Kurtosis and Confidence Interval and outliers
    skew <- psych::describe(brfss$Visits)
    ci <- pastecs::stat.desc(brfss$Visits)
    outliers <- data.frame(Lowest = head(brfss %>% distinct(Visits) %>%
                                           arrange(Visits), 5),
                           Highest =  head(brfss %>% distinct(Visits) %>%
                                             arrange(desc(Visits)), 5))
    names(outliers) <- c('Lowest', 'Highest')

    # Descriptive Statistics
    summaryWide <- brfss %>%
      dplyr::summarize(Variable = 'Visits', BRFSS = 'visits',
                N = length(Visits),
                Valid = sum(!is.na(Visits)),
                `Response Rate` = round(sum(!is.na(Visits)) /
                                          length(Visits) * 100, 2),
                Min = min(Visits, na.rm=TRUE),
                Lower = quantile(Visits, .25, na.rm = TRUE),
                Median = median(Visits, na.rm=TRUE),
                Mean = round(mean(Visits, na.rm=TRUE),2),
                Mode = getMode(as.vector(brfss$Visits[!is.na(brfss$Visits)])),
                CI = round(ci[11], 3),
                Upper = quantile(Visits, .75, na.rm = TRUE),
                Max = max(Visits, na.rm=TRUE),
                SD = round(sd(Visits, na.rm=TRUE), 2),
                SE = round(SD/sqrt(N), 2),
                Skew = round(skew$skew, 2),
                Kurtosis = round(skew$kurtosis, 2))


    # Histogram
    hist <- ggplot2::ggplot(visits,  ggplot2::aes(x =Visits, fill=..x..)) +
      ggplot2::geom_histogram(binwidth = 1) +
      ggplot2::scale_fill_gradient("Dr. Visits", low = "seagreen4", high = "royalblue4") +
      ggplot2::theme_minimal() +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = summaryWide[1,8]), color = 'royalblue4',
                 linetype = 'solid', size = .5) +
      ggplot2::labs(x = "Dr. Visits", y = 'Count') +
      ggplot2::ggtitle('Distribution of Dr. Visits Among Respondents')

    # Boxplot
    box <- ggplot2::ggplot(data = visits, ggplot2::aes(x = 'Visits', y=Visits)) +
      ggplot2::geom_boxplot(fill = 'seagreen4') +
      ggplot2::ggtitle("Boxplot of Dr. Visits in Previous 12 Months") +
      ggplot2::theme_minimal() +
      ggplot2::geom_point(data = data.frame(Visits = mean(visits$Visits)),
                 ggplot2::aes(x ='Visits', y=Visits)) +
      ggplot2::theme(text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::labs(x = 'Dr. Visits' ,
           y = 'Distribution of Dr. Visits') +
      ggplot2::theme(axis.text.y= ggplot2::element_blank(),
            legend.position = 'none')

    analysis <- list(
      stats = summaryWide,
      hist = hist,
      box = box,
      outliers = outliers
    )

    return(analysis)

  }

  # Perform Analysis
  depression <- analyzeDepression(brfss)
  sickDays <- analyzeSickDays(brfss)
  sickDaysRange <- analyzeSickDaysRange(brfss)
  income <- analyzeIncome(brfss)
  education <- analyzeEducation(brfss)
  chronic <- analyzeChronic(brfss)
  visits <- analyzeVisits(brfss)

  # Summarize Categorical Analysis
  categorical <- rbind(depression$summary, income$summary,
                       education$summary, chronic$summary)
  quantitative <- rbind(sickDays$stats, visits$stats)

  univariate <- list(
    depression = depression,
    sickDays = sickDays,
    sickDaysRange = sickDaysRange,
    income = income,
    education = education,
    chronic = chronic,
    visits = visits,
    categorical = categorical,
    quantitative = quantitative
  )

  return(univariate)

}
