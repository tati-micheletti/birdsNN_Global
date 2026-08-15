fitBaselineResiduals <- function(territoryData, euringCode){
  
  results <- lapply(euringCode, function(code){

    sub <- territoryData[EURING %in% code, ]

    poissonFit <- glm(Reviere ~ factor(year), data = sub, family = poisson())

    # H0: equidispersion (Poisson is fine). H1: NB-shaped variance (mu + alpha*mu^2).
    # Only escalate to glm.nb() if this comes back significant.
    dispTest <- AER::dispersiontest(poissonFit, trafo = NULL, alternative = "greater")

    if (dispTest$p.value < 0.05){
      fit <- MASS::glm.nb(Reviere ~ factor(year), data = sub)
      distributionUsed <- "negative binomial"
    } else {
      fit <- poissonFit
      distributionUsed <- "Poisson"
    }

    sub[, residual := residuals(fit, type = "deviance")]

    list(euringCode        = code,
         model             = fit,
         distributionUsed  = distributionUsed,
         dispersionTest    = list(statistic = unname(dispTest$statistic),
                                  pValue    = dispTest$p.value),
         residualTable     = sub[, .(Nummer, ROUTENCODE, year, Reviere, residual)])

  })

  names(results) <- as.character(euringCode)

  return(results)
}