## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(9, 6)
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(optconerrf)

## ----message = FALSE, results = "markup"--------------------------------------
design <- getDesignOptimalConditionalErrorFunction(
  alpha = 0.025, alpha1 = 0.0154, alpha0 = 0.5, conditionalPower = 0.9,
  delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
  firstStageInformation = 50, useInterimEstimate = FALSE
)

## ----message = FALSE, results = "markup"--------------------------------------
# Comprehensive design output
print(design)

## ----fig.dim = c(6,4)---------------------------------------------------------
plot(design) # Plot of the optimal conditional error function

## ----fig.dim = c(6,4)---------------------------------------------------------
plot(design, type = 2) # Plot of the second-stage information

## -----------------------------------------------------------------------------
design$alpha
design$levelConstant
print(design$levelConstant, digits = 12) # For more precision

## -----------------------------------------------------------------------------
getOptimalConditionalError(
  firstStagePValue = 0.1, design = design
)

## -----------------------------------------------------------------------------
getOptimalConditionalError(
  firstStagePValue = c(0.0005, 0.1, 0.05, 0.5, 0.8),
  design = design
)

## -----------------------------------------------------------------------------
getSecondStageInformation(
  firstStagePValue = 0.1, design = design
)

getSecondStageInformation(
  firstStagePValue = c(0.0005, 0.1, 0.05, 0.5, 0.8),
  design = design
)

## -----------------------------------------------------------------------------
getExpectedSecondStageInformation(
  design = design, likelihoodRatioDistribution = "fixed", deltaLR = 0
)

## -----------------------------------------------------------------------------
getExpectedSecondStageInformation(
  design = design, likelihoodRatioDistribution = "fixed", deltaLR = 0.25
)

## -----------------------------------------------------------------------------
getExpectedSecondStageInformation(
  design = design
)

## -----------------------------------------------------------------------------
getOverallPower(
  design, alternative = c(0, 0.25))

## -----------------------------------------------------------------------------
summary(design)

