## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(6,4)
)

## ----setup--------------------------------------------------------------------
library(optconerrf)

## -----------------------------------------------------------------------------
trialDesignConstraints <- getDesignOptimalConditionalErrorFunction(
  alpha = 0.025,
  alpha1 = 0.000158,
  alpha0 = 0.5,
  conditionalPower = 0.9,
  delta1 = 0.25,
  useInterimEstimate = FALSE,
  likelihoodRatioDistribution = "maxlr",
  firstStageInformation = 85,
  minimumSecondStageInformation = 85/2,
  maximumSecondStageInformation = 85*2
)

## ----fig.dim = c(6,4)---------------------------------------------------------
# type = 2 for second-stage information
plot(trialDesignConstraints, type = 2) +
  ggplot2::geom_hline(yintercept = c(85/2, 85*2), linetype = "dashed", colour = "gray")

## -----------------------------------------------------------------------------
trialDesignConstraintsConditionalError <- getDesignOptimalConditionalErrorFunction(
  alpha = 0.025,
  alpha1 = 0.000158,
  alpha0 = 0.5,
  conditionalPower = 0.9,
  delta1 = 0.25,
  useInterimEstimate = FALSE,
  likelihoodRatioDistribution = "maxlr",
  firstStageInformation = 85,
  minimumConditionalError =  0.02396,
  maximumConditionalError = 0.36382
)

## -----------------------------------------------------------------------------
getExpectedSecondStageInformation(
  trialDesignConstraints, 
  likelihoodRatioDistribution = "fixed", 
  deltaLR = 0.25
)
getExpectedSecondStageInformation(
  trialDesignConstraintsConditionalError, 
  likelihoodRatioDistribution = "fixed",
  deltaLR = 0.25
)

## -----------------------------------------------------------------------------
trialDesignInterimEstimate <- 
  getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.9,
    useInterimEstimate = TRUE,
    delta1Min = 0.15,
    delta1Max = 0.35,
    firstStageInformation = 85,
    likelihoodRatioDistribution = "maxlr"
  )

## -----------------------------------------------------------------------------
plot(trialDesignInterimEstimate, type = 1)
plot(trialDesignInterimEstimate, type = 2)

## -----------------------------------------------------------------------------
myConditionalPowerFunction <- function(firstStagePValue) {
  return(pnorm(1 - firstStagePValue))
}

## -----------------------------------------------------------------------------
p1 <- seq(0, 1, 0.01)
plot(p1, myConditionalPowerFunction(p1), type = "l")
abline(h = myConditionalPowerFunction(c(0, 1)), col = "blue")

## -----------------------------------------------------------------------------
trialDesignConditionalPowerFunction <- 
  getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPowerFunction = myConditionalPowerFunction,
    delta1 = 0.25,
    useInterimEstimate = FALSE,
    firstStageInformation = 100,
    likelihoodRatioDistribution = "maxlr"
  )

## -----------------------------------------------------------------------------
plot(trialDesignConditionalPowerFunction, type = 1)

## -----------------------------------------------------------------------------
plot(trialDesignConditionalPowerFunction, type = 2)

## -----------------------------------------------------------------------------
combinedDesign <- 
  getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPowerFunction = myConditionalPowerFunction,
    delta1Min = 0.25,
    useInterimEstimate = TRUE,
    firstStageInformation = 100,
    likelihoodRatioDistribution = "maxlr",
    minimumSecondStageInformation = 50
  )

## -----------------------------------------------------------------------------
plot(combinedDesign)

