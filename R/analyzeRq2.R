#==============================================================================#
#                               analyzeRq2.R                                   #
#==============================================================================#
#'
#' analyzeRq2
#'
#' \code{analyzeRq2(brfss)}{Performs analyses for research question #2,
#' Is there a relationship between depression and chronic illness?}
#'
#' This function extracts frequency and proportion data, renders frequency
#' and proportion plots, conducts tests of marginal independence, and
#' calculates odds ratios.
#'
#'
#' @param brfss Data frame of preprocessed 2013 Behavioral Risk Factor
#' Surveillance System (BRFSS) telephone survey.
#' @return analysis List containing frequency and proportion data, plots,
#'     and statistical tests. The variables are as follows:
#'     \itemize{
#'      \itemize{
#'       \item{r2}{Frequencies of depression and chronic illness diagnoses}
#'       \item{freq}{Frequencies of depression and chronic illness diagnoses in table format}
#'       \item{prop}{Proportions of depression and chronic illness diagnoses in table format}
#'      }
#'      \itemize{
#'      \item{freq}{Frequencies of depression and chronic illness diagnoses in data frame format}
#'      \item(prop){Proportions of depression and chronic illness diagnoses in data frame format}
#'      }
#'      \item{freq}{Depression and chronic illness frequencies bar plot}
#'      \item{prop}{Depression and chronic illness proportions bar plot}
#'      }
#'      \itemize{
#'      \item{X2}{Chisq test of marginal association}
#'      \item{assoc}{The strength of the association}
#'      \item{odds}{The odds ratios}
#'      }
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
analyzeRq2 <- function(brfss) {

  #---------------------------------------------------------------------------#
  #                                Get Data                                   #
  #---------------------------------------------------------------------------#

  # Get Data of interest
  rq2Data <- brfss %>%
    filter(!is.na(Depression) & !is.na(Chronic)) %>%
    select(Depression,  Chronic)

  #---------------------------------------------------------------------------#
  #                              Create Tables                                #
  #---------------------------------------------------------------------------#
  # Create Tables for Analysis of Effect of Depression on Productivity        #
  #---------------------------------------------------------------------------#
  r2Freq <- with(rq2Data, table(Depression, Chronic))
  r2FreqTbl <- ftable(r2Freq)
  r2FreqDf <- as.data.frame(r2FreqTbl)
  r2PropTbl <- ftable(prop.table(r2FreqTbl, 1))
  r2PropDf <- as.data.frame(r2PropTbl)

  # Format 2-Way Depression Marginal Frequency Contingency Tables for plotting
  r2FreqDf <-r2FreqDf %>% arrange(Depression, desc(Chronic)) %>%
    group_by(Depression) %>% mutate(cumFreq = cumsum(Freq),
                                    pos = cumFreq - 0.5 * Freq)

  # Format 2-Way Depression Marginal Proportion Contingency Tables for plotting
  r2PropDf <-r2PropDf %>% arrange(Depression, desc(Chronic)) %>%
    group_by(Depression) %>% mutate(pct = round(Freq * 100, 0),
                                    cumPct = cumsum(pct),
                                    pos = cumPct - 0.5 * pct)

  #---------------------------------------------------------------------------#
  #                            Create  Plots                                  #
  #---------------------------------------------------------------------------#
  # Create Contingency Table
  r2FreqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Depression, y = Freq, fill = Chronic),
             data = r2FreqDf, stat = 'identity') +
    ggplot2::geom_text(data = r2FreqDf, ggplot2::aes(x = Depression, y = pos,
                                           label = Freq),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Chronic Illness") +
    ggplot2::labs(title = "Frequencies",
         x = "Diagnosis of Depression",
         y = 'Count')

  #---------------------------------------------------------------------------#
  # Create Marginal Proportion Bar Plot of Sick Days Indicator by Depression
  r2PropBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Depression, y = pct, fill = Chronic),
             data = r2PropDf, stat = 'identity') +
    ggplot2::geom_text(data = r2PropDf, ggplot2::aes(x = Depression, y = pos,
                                           label = paste0(pct,"%")),
              colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
          text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Chronic Ilness") +
    ggplot2::labs(title = "Proportions",
         x = "Diagnosis of Depression",
         y = 'Proportion')


  #---------------------------------------------------------------------------#
  #                           Conduct Tests                                   #
  #---------------------------------------------------------------------------#
  # Conduct chi-square test of independence of
  r2X2 <- stats::chisq.test(r2FreqTbl)
  r2Assoc <- vcd::assocstats(r2FreqTbl)

  #---------------------------------------------------------------------------#
  #                         Create Odds Ratios                                #
  #---------------------------------------------------------------------------#
  # Computation of Odds of Depression and Chronic Illness
  r2Odds <- vcd::oddsratio(r2FreqTbl, stratum = NULL, log = FALSE)
  dims <- names(r2Odds$coefficients)
  odds <- exp(as.numeric(r2Odds$coefficients))
  r2Odds <- data.frame(Dimension = dims, Odds = odds)


  #---------------------------------------------------------------------------#
  #                     Format and Return Results                             #
  #---------------------------------------------------------------------------#
  analysis <- list(
    tables = list(
      r2 = r2Freq,
      freq = r2FreqTbl,
      prop = r2PropTbl
    ),
    dataFrames = list(
      freq = r2FreqDf,
      prop = r2PropDf
    ),
    plots = list(
      freq = r2FreqBar,
      prop = r2PropBar
    ),
    tests = list(
      X2 = r2X2,
      assoc = r2Assoc,
      odds = r2Odds
    )
  )
  return(analysis)

}
## ---- end

