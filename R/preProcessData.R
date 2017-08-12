#==============================================================================#
#                                 preProcessData                               #
#==============================================================================#
#'  preProcessData
#'
#'  \code{preProcessData}
#'
#' This function preprocesses the BRFSS data for downstream analysis.
#'
#' @param brfss Data frame of 2013 Behavioral Risk Factor Surveillance System (BRFSS) data
#' @return brfss Data frame containing variables required for downstream analysis
#' @author John James, \email{jjames@@dataference.com}
#' @export
preProcessData <- function(brfss) {

  # Code new 'chronic' variable
  brfss <- data.table::as.data.table(brfss)
  chronic <- subset(brfss, select = c(cvdinfr4, cvdcrhd4, cvdstrk3, asthma3,
                                      chcscncr,chcocncr, chccopd1, havarth3, chckidny,
                                      diabete3))
  chronic <- data.table::as.data.table(chronic)

  brfss$chronicCount <- apply(X=chronic,1,FUN=function(x) length(which(x=='Yes')))
  data.table:::`[.data.table`(brfss, j =  chronic := ifelse(chronicCount > 0, 'Yes', 'No'))
  brfss$chronic <- factor(brfss$chronic, levels = c('Yes', 'No'))

  # Obtain variables of interest
  brfss <- brfss %>% select(cvdinfr4, cvdcrhd4, cvdstrk3, asthma3,
                            chcscncr,chcocncr, chccopd1, havarth3, chckidny,
                            diabete3,addepev2, poorhlth, X_incomg, X_educag,
                            drvisits, chronic) %>%
    dplyr::rename('Heart Attack' = cvdinfr4,
                  'Angina Or Coronary Heart Disease' = cvdcrhd4,
                  Stroke = cvdstrk3, Asthma = asthma3, 'Skin Cancer' = chcscncr,
                  'Cancer (Other)' = chcocncr, COPD = chccopd1,
                  Arthritis = havarth3, 'Kidney Disease' = chckidny,
                  Diabetes = diabete3, Depression = addepev2,
                  SickDays = poorhlth, Income = X_incomg, Education = X_educag,
                  Visits = drvisits, Chronic = chronic) %>%
    mutate(SickDaysInd = ifelse(SickDays > 0, 'Yes', 'No'),
             Diabetes = ifelse(grepl("No", Diabetes), "No", "Yes" ))

  # Remove currency symbol from income category and reorder factor levels
  brfss$Income <- gsub("[\\$]", "", brfss$Income)
  brfss$Income <- factor(brfss$Income,
                         levels = c('Less than 15,000',
                                    '15,000 to less than 25,000',
                                    '25,000 to less than 35,000',
                                    '35,000 to less than 50,000',
                                    '50,000 or more'))


  # Create date range category variable
  brfss <- brfss %>% mutate(SickDaysRange =
                              cut(SickDays, c(-1,5,10,15,20,25,30),
                                  labels = c('0-5 Days', '6-10 Days',
                                             '11-15 Days', '16-20 Days',
                                             '21-25 Days', '26-30 Days')))
  return(brfss)

}
## ---- end
