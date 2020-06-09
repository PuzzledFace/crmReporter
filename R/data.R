#' Utility function to derive estimated probabilties of DLT for the example
#' dataset based on example 1 in O'Quigley et al (1990) [pp40-41]
#' @param d the runjags tibble to be augmented
#' @param doseGrid the doses for which p(DLT) should be calculated for each
#' MCMC sample
#' @return the augmented tibble
#' @export
augmentOQuigleyData <- function(d, doseGrid=1:6) {
  xhat <- atanh(2*c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7)-1) # First table on p41
  d <- d %>%
    dplyr::select(Chain, Sample) %>%
    tidyr::expand(
      tidyr::nesting(Chain, Sample),
      Dose=doseGrid
    )  %>%
    dplyr::left_join(d, by=c("Chain", "Sample")) %>%
    dplyr::group_by(Chain, Sample) %>%
    tidyr::spread(key=Parameter, value=.data$Value) %>%
    dplyr::mutate(
      XHat=xhat[Dose],
      Prob=((tanh(XHat) + 1)/2)^.data$a
    ) %>%
    tidyr::pivot_longer(names_to="Parameter", values_to="Value", cols=c(.data$a, .data$Prob)) %>%
    dplyr::mutate(Index=NA) %>%
    dplyr::arrange(Chain, Sample, Dose, Parameter, Index) %>%
    dplyr::ungroup()
  return(d)
}

#' Utility function to derive estimated probabilties of DLT (Prob2) and DLT or
#' sub-DLT for the example oCRM dataset
#' @param d the runjags tibble to be augmented
#' @param doseGrid the doses for which p(DLT) and p(Sub-DLT or DLT) should be
#' calculated for each MCMC sample
#' @return the augmented tibble
#' @export
augmentOCRMData <- function(d, doseGrid=1:10) {
  d <- d %>%
    dplyr::select(Chain, Sample) %>%
    tidyr::expand(
      tidyr::nesting(Chain, Sample),
      Dose=doseGrid
    )  %>%
    dplyr::left_join(d, by=c("Chain", "Sample")) %>%
    dplyr::group_by(Chain, Sample) %>%
    dplyr::mutate(
      Temp=ifelse(is.na(Index), Parameter, paste0(Parameter, "_", Index))
    ) %>%
    dplyr::select(-Parameter, -Index) %>%
    tidyr::spread(key=Temp, value=.data$Value) %>%
    dplyr::mutate(
      XHat=Dose-11,
      Z1=.data$alpha_1 + beta*XHat,
      Z2=.data$alpha_2 + beta*XHat,
      Prob_1=exp(Z1)/(1 + exp(Z1)),
      Prob_2=exp(Z2)/(1 + exp(Z2))
    ) %>%
    dplyr::select(-Z1, -Z2) %>%
    tidyr::pivot_longer(
      names_to="Parameter",
      values_to="Value",
      cols=c(.data$alpha_1, .data$alpha_2, .data$beta, .data$Prob_1, .data$Prob_2)
    ) %>%
    tidyr::separate(
      col=Parameter,
      into=c("Parameter", "Index", NA),
      sep="_",
      convert=TRUE,
      fill="right") %>%
    dplyr::arrange(Chain, Sample, Dose, Parameter, Index) %>%
    dplyr::ungroup()
  return(d)
}

#' Utility function to derive estimated probabilties of DLT toxicity band
#' membership for the example nCRM dataset
#' @param d the runjags tibble to be augmented
#' @param doseGrid the doses for which p(DLT) and p(Sub-DLT or DLT) should be
#' calculated for each MCMC sample
#' @return the augmented tibble
#' @export
augmentNCRMData <- function(d,
                            doseGrid=c(3, 6, 8, 12, 16, 21, 28, 37, 46, 61, 80)) {
  d <- d %>%
    dplyr::select(Chain, Sample) %>%
    tidyr::expand(tidyr::nesting(Chain, Sample),
                  Dose=doseGrid) %>%
    dplyr::left_join(d,
                     by=c("Chain", "Sample")) %>%
    dplyr::group_by(Chain, Sample) %>%
    dplyr::mutate(Temp=ifelse(is.na(Index),
                              Parameter,
                              paste0(Parameter, Index))) %>%
    dplyr::select(-Parameter, -Index) %>%
    tidyr::spread(key=Temp, value=.data$Value) %>%
    dplyr::mutate(XHat=log(Dose/21),
                  Z1=alpha + beta*XHat,
                  Prob=exp(Z1)/(1 + exp(Z1)),
                  Band=1*(Prob < 0.15) +
                       2*(Prob > 0.15 & Prob <= 0.33) +
                       3*(Prob > 0.33 & Prob <= 0.60) +
                       4*(Prob > 0.6)) %>%
    dplyr::select(Chain, Sample, Dose, XHat, Prob, Band) %>%
    dplyr::ungroup()
  return(d)
}

#' Utility Function to Fit a JAGS Model
#'
#' The \code{observedData} tibble is converted to a list and the \code{extras}
#' list is then appended to it.  An additional element, "n", containing the
#' number of rows in \code{observdData} is added, and the combined list is
#' passed to \code{runjags} the \code{data} argument.
#' @param modelString  the JAGS model string
#' @param observedData a tibble containing the observed data
#' @param extras additional elements of the named list pssed to \code{runjags}
#' @param inits the value of the \code{inits} parameter passed to \code{runjags}
#' @param auto use \code{autorun.jags} or \code{run.jags}?
#' @param ... additional arguments passed to \code{runjags}
#' @return the runjags object produced by JAGS
#' @export
fitModel <- function(modelString,
                     observedData,
                     extras=list(),
                     inits=list(),
                     auto=FALSE,
                     ...) {
  d <- extras
  d <- append(d, as.list(observedData))
  d <- append(d, list("n"=nrow(observedData)))
  if (auto) {
    rv <- runjags::autorun.jags(modelString, data=d, inits=inits, ...)
  } else {
    rv <- runjags::run.jags(modelString, data=d, inits=inits, ...)
  }
  return (rv)
}

#' Utility Function to Obtain a JAGS Model String
#'
#' Obtain the model string for one of the example trials included with
#' crmReporter.
#' @param s the key to identified the required model string.
#' @return The requested model string
#' @examples
#' model <- getModelString("oquigley")
#' @export
getModelString <- function(s=c("oquigley", "ocrm", "crm")) {
  if (s == "oquigley")
    return ("model {
                #Prior
                a ~ dexp(1)
                #Likelihood
                for (i in 1:n) {
                  Toxicity[i] ~ dbern(((tanh(XHat[i]) + 1)/2)**a)
                }
              }
              #monitor# a
              ")
  if (s == "ocrm")
    return ("data {
               for (i in 1:length(XHat)) {
                 for (j in 1:2) {
                   DLT[i, j] <- Toxicity[i] >= j
                 }
               }
             }
             model {
               alpha[1] ~ dnorm(meanAlpha1, 1/(sdAlpha1*sdAlpha1))  #  JAGS uses precision, not variance
               alpha[2] ~ dnorm(meanAlpha2, 1/(sdAlpha2*sdAlpha2))  #  JAGS uses precision, not variance
               #Common slope.  LogNormal distribution ensures slope is positive
               gamma ~ dnorm(meanLogBeta, 1/(sdLogBeta*sdLogBeta))  #  JAGS uses precision, not variance
               beta <- exp(gamma)
               for (i in 1:n) {
                 for (j in 1:2) {
                   z[i, j] <- alpha[j] + beta * XHat[i]
                   p[i, j] <- exp(z[i, j]) / (1 + exp(z[i, j]))
                   DLT[i, j] ~ dbern(p[i, j])
                 }
               }
             }
             #monitor# alpha[1], alpha[2], beta
          ")
  if (s == "crm")
    return ("model {
               theta ~ dmnorm(mean[], precision[,])
               alpha <- theta[1]
               beta <- exp(theta[2])
               for (i in 1:n) {
                 z[i] <- alpha + beta * XHat[i]
                 p[i] <- exp(z[i]) / (1 + exp(z[i]))
                 Toxicity[i] ~ dbern(p[i])
               }
             }
             #monitor# alpha, beta
           ")
  stop(paste0("Unable to return model string for key '", s, "'."))
}

#' Documentation for datasets in /data and /data-raw
#'
#' Patient data from O'Quigley J, Pepe M & Fisher L (1990) A Practical Design for
#' Phase 1 Clinical Trials in Cancer.  Biometrics 46:1 33-48.  Example 1
#'
#' @format A tibble with 25 rows and 4 columns
#' \describe{
#'   \item{Cohort}{The cohort number, here, identical to the SubjectID as the model was updatd after every subject}
#'   \item{SubjectID}{The subject id}
#'   \item{Dose}{The index of the dose received by this subject.  The actual doses used are never defined in the paper}
#'   \item{Toxicity}{A binary flag indicating whether or not the subject experienced a DLT. [0=No, 1=Yes]}
#' }
#' @source \url{https://pubmed.ncbi.nlm.nih.gov/2350571/}
"oQuigleyPatientData"

#' Patient data from a fictitious oCRM trial
#'
#' @format A tibble with 15 rows and 5 columns
#' \describe{
#'   \item{Cohort}{The cohort number, here, identical to the SubjectID as the model was updatd after every subject}
#'   \item{SubjectID}{The subject id}
#'   \item{Dose}{The index of the dose received by this subject.}
#'   \item{XHat}{The nominal dose, on the x-hat scale, received by this subject.}
#'   \item{Toxicity}{A binary flag indicating whether or not the subject experienced a DLT. [0=No, 1=Yes]}
#' }
"oCRMPatientData"

#' Patient data from a fictitious nCRM trial
#'
#' @format A tibble with 20 rows and 5 columns
#' \describe{
#'   \item{Cohort}{The cohort number, here, identical to the SubjectID as the model was updatd after every subject}
#'   \item{SubjectID}{The subject id}
#'   \item{Dose}{The milligram dose received by this subject.}
#'   \item{XHat}{The nominal dose, on the x-hat scale, received by this subject.}
#'   \item{Toxicity}{A binary flag indicating whether or not the subject experienced a DLT. [0=No, 1=Yes]}
#' }
"nCRMPatientData"

#' The sampled posterior for the \code{oQuigleyPatientData} dataset
#'
#' A runjags object containing the output from runjags when run on the example O'Quigley dataset with a chain length of 20000
#'
#' @format a runjags object with 3 chains
"oQuigleyPosterior"

#' The sampled posterior for the \code{oQuigleyPatientData} dataset
#'
#' A runjags object containing the output from runjags when run on the example O'Quigley dataset with a chain length of 2000
#'
#' @format a runjags object with 3 chains
"oQuigleyPosteriorShort"

#' The sampled posterior for the \code{nCRMPatientData} dataset
#'
#' A runjags object containing the output from runjags when run on the example O'Quigley dataset with a chain length of 2000
#'
#' @format a runjags object with 3 chains
"nCRMPosteriorShort"

#' The sampled posterior for the \code{nCRMPatientData} dataset
#'
#' A runjags object containing the output from runjags when run on the example O'Quigley dataset with a chain length of 30000
#'
#' @format a runjags object with 3 chains
"nCRMPosterior"

#' The sampled posterior for the \code{oCRMPatientData} dataset
#'
#' A runjags object containing the output from runjags when run on the example O'Quigley dataset with a chain length of 2000
#'
#' @format a runjags object with 3 chains
"oCRMPosteriorShort"

#' The sampled posterior for the \code{oCRMPatientData} dataset
#'
#' A runjags object containing the output from runjags when run on the example O'Quigley dataset with a chain length of 30000
#'
#' @format a runjags object with 3 chains
"oCRMPosterior"

