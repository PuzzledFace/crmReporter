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
    dplyr::mutate(XHat=Dose-11,
                  Z1=alpha1 + beta*XHat,
                  Z2=alpha2 + beta*XHat,
                  Prob1=exp(Z1)/(1 + exp(Z1)),
                  Prob2=exp(Z2)/(1 + exp(Z2))) %>%
    dplyr::select(Chain, Sample, Dose, XHat, Prob1, Prob2) %>%
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

#' Generate the patient data and corresponding runjags Object for O'Quigley
#'
#' Patient level data and MCMC samples for Example 1 in O'Quigley et al (1990).
#'
#' @param chainLength the number of samples to generate in each chain
#' @return a list with two elements.  Item "patients" contains the input data;
#' item "posterior" contains the runjags object
#' @section Usage notes:
#' The runjags object is large - about xxxxMB - so it is generated by this function
#' rather than being provided as a saved object.
#' @examples
#' x <- generateOQuigleyData(chainLength=1000)
#' @export
generateOQuigleyData <- function(chainLength=10000) {
  if (!requireNamespace("runjags", quietly=TRUE)) {
    stop("The runjags package is not available.  Unable to continue.")
  }
  oQuigleyPatientData <- tibble::tibble(Cohort=1:25,
                                       SubjectID=1:25,
                                       Dose=    c(3,4,4,3,3,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1),
                                       Toxicity=c(0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1,1))

  a <- 1
  xhat <- atanh(2*c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7)**(1/a)-1)
  oQuigleyPatientData <- oQuigleyPatientData %>% dplyr::mutate(XHat = xhat[Dose])

  oQuigleyPosterior <-
    fitModel(getModelString("oquigley"),
             oQuigleyPatientData %>% dplyr::select(XHat, Toxicity),
             inits=list(list(a=1), list(a=10), list(a=0.1)),
             sample=chainLength)

  rv <- list("patients"=oQuigleyPatientData,
             "posterior"= oQuigleyPosterior)
  return(rv)
}


#' Generate patient data and the corresponding runjags Object Containing the
#' Results of a Fictitious oCRM Trial
#' @param chainLength the number of samples to generate in each chain
#' @return a list with two elements.  Item "patients" contains the input data;
#' item "posterior" contains the runjags object
#' @section Usage notes:
#' The runjags object is large - about 1MB - so it is generated by this function
#' rather than being provided as a saved object.
#' @examples
#' x <- generateOCRMData(chainLength=1000)
#' @export
generateOCRMData <- function(chainLength=120000) {
  if (!requireNamespace("runjags", quietly=TRUE)) {
    stop("The runjags package is not available.  Unable to continue.")
  }

  oCRMPatientData <- tibble::tibble(Cohort=1:15,
                                    SubjectID=1:15,
                                    Dose=    c(  1,   1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  6,  6,  6),
                                    XHat=    c(-10, -10, -9, -9, -8, -8, -7, -7, -6, -6, -5, -5, -5, -5, -5),
                                    Toxicity=c(  0,   0,  0,  0,  0,  0,  0,  0,  1,  2,  2,  1,  1,  1,  1))

  oCRMPosterior <-
    fitModel(getModelString("ocrm"),
             oCRMPatientData %>% dplyr::select(XHat, Toxicity),
             sample=chainLength,
             thin=2,
             inits=list(list(alpha=c(5, 3), gamma=0),
                        list(alpha=c(0, 0), gamma=3),
                        list(alpha=c(-2, 3), gamma=1)),
             extras=list("meanAlpha1"=4,
                         "meanAlpha2"=3,
                         "meanLogBeta"=log(1),
                         "sdAlpha1"=3,
                         "sdAlpha2"=4,
                         "sdLogBeta"=3))

  rv <- list("patients"=oCRMPatientData, "posterior"= oCRMPosterior)
  return(rv)
}

#' Generate a runjags Object Containing the Results of an nCRM Trial
#' @param chainLength the number of samples to generate in each chain
#' @return a list with two elements.  Item "patients" contains the input data;
#' item "posterior" contains the runjags object
#' @section Usage notes:
#' The runjags object is large - about 10MB - so it is generated by this function
#' rather than being provided as a saved object.
#' @examples
#' x <- generateNCRMData(chainLength=10000)
#' @export
generateNCRMData <- function(chainLength=60000) {
  if (!requireNamespace("runjags", quietly=TRUE)) {
    stop("The runjags package is not available.  Unable to continue.")
  }

  nCRMPatientData <- tibble::tibble(Cohort=c(1:7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10, 11, 11, 11),
                                    SubjectID=1:20,
                                    Dose=c(3, 6, 8, 12, 16, 21, 21, 21, 16, 16, 16, 12, 12, 12, 16, 16, 16, 16, 16, 16),
                                    XHat=log(Dose/21),
                                    Toxicity=c( 0, 0, 0,  0,  0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))

  mean <- c(0, 1)
  precision <- solve(matrix(c(1, -0.1, -0.1, 1), ncol=2))
      nCRMPosterior <-
        fitModel(getModelString("crm"),
                 nCRMPatientData %>% dplyr::select(XHat, Toxicity),
                 inits=list(list(theta=c(1,log(4))),
                            list(theta=c(0,log(1))),
                            list(theta=c(-2,log(7)))),
                 extras=list("precision"=precision,
                             "mean"=mean),
                 sample=chainLength,
                 thin=2)

  rv <- list("patient"=nCRMPatientData, "posterior"=nCRMPosterior)
  return(rv)
}

