#==============================================================================#
#                             estimateSampleSize.R                             #
#==============================================================================#
#' Sample size estimate
#'
#' \code{estimateSampleSize(effect, sigLevel, powerLevel)}
#'
#' Function to compute minimum sample size required for
#' Variable and partial response rates may affect sample size. To ensure that
#' readers could rely upon non-significant results, this analyst conducted several
#' power analyses using the pwr package (Ekstrom et al. 2017). Each test
#' indicated the minimum sample size required to detect a small effect
#' of 0.1, with the p<.05 probability of a type I error of p < .05
#' and power of 0.8
#'
#' @param effect Numeric variable indicating effect size to discern
#' @param sigLevel Numeric variable indicating the level of significance
#' @param powerLevel Numeric indicator of the power level
#' @return power Data frame indicating minimum sample sizes for various statistical tests
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
estimateSampleSize <- function(effect = 0.1, sigLevel = 0.05, powerLevel = 0.8) {
  x2Income <- pwr::pwr.chisq.test(w = effect, sig.level = sigLevel, power = powerLevel, df = 4)
  x2Education <- pwr::pwr.chisq.test(w = effect, sig.level = sigLevel, power = powerLevel, df = 3)
  t2 <- pwr::pwr.t.test(d = effect, sig.level = sigLevel, power = powerLevel, alternative = 'two.sided')
  anv <- pwr::pwr.anova.test(k = 2, f = effect, power = powerLevel)

  methods <- c(paste("Income", x2Income$method), paste("Education", x2Education$method), t2$method, anv$method)
  N <- c(x2Income$N, x2Education$N, t2$n, anv$n)
  pwr <- c(x2Income$power, x2Education$power, t2$power, anv$power)
  sig <- c(x2Income$sig.level, x2Education$sig.level, t2$sig.level, anv$sig.level)
  power <- data.frame(Method = methods, N = N, Power = pwr, Significance = sig)

  return(power)
}
