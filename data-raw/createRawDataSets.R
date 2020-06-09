#' O'Quigley et al (1980)
oQuigleyPatientData <- tibble::tibble(
                         Cohort=1:25,
                         SubjectID=1:25,
                         Dose=    c(3,4,4,3,3,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1),
                         Toxicity=c(0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1,1)
                       )
usethis::use_data(oQuigleyPatientData, overwrite=TRUE)

#` Fictitious oCRM trial
usethis::use_data(
  tibble::tibble(
    Cohort=1:15,
    SubjectID=1:15,
    Dose=    c(  1,   1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  6,  6,  6),
    XHat=    c(-10, -10, -9, -9, -8, -8, -7, -7, -6, -6, -5, -5, -5, -5, -5),
    Toxicity=c(  0,   0,  0,  0,  0,  0,  0,  0,  1,  2,  2,  1,  1,  1,  1)
  ),
  overwrite=TRUE
)

usethis::use_data(
  tibble::tibble(
    Cohort=c(1:7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10, 11, 11, 11),
    SubjectID=1:20,
    Dose=c(3, 6, 8, 12, 16, 21, 21, 21, 16, 16, 16, 12, 12, 12, 16, 16, 16, 16, 16, 16),
    XHat=log(Dose/21),
    Toxicity=c( 0, 0, 0,  0,  0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0)
  ),
  overwrite=TRUE
)

# Obtain posteriors
if (!requireNamespace("runjags", quietly=TRUE)) {
  stop("The runjags package is not available.  Unable to continue.")
}
source ("./R/functions.R", echo=FALSE)
source ("./R/data.R", echo=FALSE)

a <- 1
xhat <- atanh(2*c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7)**(1/a)-1)
oQuigleyPatientData <- oQuigleyPatientData %>% dplyr::mutate(XHat = xhat[Dose])

oQuigleyPosterior <-
  fitModel(getModelString("oquigley"),
           oQuigleyPatientData %>% dplyr::select(XHat, Toxicity),
           inits=list(list(a=1), list(a=10), list(a=0.1)),
           sample=20000)
usethis::use_data(oQuigleyPosterior, overwrite=TRUE)

oQuigleyPosteriorShort <-
  fitModel(getModelString("oquigley"),
           oQuigleyPatientData %>% dplyr::select(XHat, Toxicity),
           inits=list(list(a=1), list(a=10), list(a=0.1)),
           sample=2000)
usethis::use_data(oQuigleyPosteriorShort, overwrite=TRUE)

oCRMPosterior <-
  fitModel(
    getModelString("ocrm"),
      oCRMPatientData %>% dplyr::select(XHat, Toxicity),
      sample=30000,
      thin=2,
      inits=list(
        list(alpha=c(5, 3), gamma=0),
        list(alpha=c(0, 0), gamma=3),
        list(alpha=c(-2, 3), gamma=1)
      ),
      extras=list(
        "meanAlpha1"=4,
        "meanAlpha2"=3,
        "meanLogBeta"=log(1),
        "sdAlpha1"=3,
        "sdAlpha2"=4,
        "sdLogBeta"=3)
      )
usethis::use_data(oCRMPosterior, overwrite=TRUE)

oCRMPosteriorShort <-
  fitModel(
    getModelString("ocrm"),
    oCRMPatientData %>% dplyr::select(XHat, Toxicity),
    sample=2000,
    thin=2,
    inits=list(
      list(alpha=c(5, 3), gamma=0),
      list(alpha=c(0, 0), gamma=3),
      list(alpha=c(-2, 3), gamma=1)
    ),
    extras=list(
      "meanAlpha1"=4,
      "meanAlpha2"=3,
      "meanLogBeta"=log(1),
      "sdAlpha1"=3,
      "sdAlpha2"=4,
      "sdLogBeta"=3)
  )
usethis::use_data(oCRMPosteriorShort, overwrite=TRUE)

nCRMPosterior <-
  fitModel(
    getModelString("crm"),
    nCRMPatientData %>% dplyr::select(XHat, Toxicity),
    inits=list(
      list(theta=c(1,log(4))),
      list(theta=c(0,log(1))),
      list(theta=c(-2,log(7)))
    ),
    extras=list(
      "precision"=solve(matrix(c(1, -0.1, -0.1, 1), ncol=2)),
      "mean"=c(0, 1)),
      sample=30000,
      thin=2
    )
usethis::use_data(nCRMPosterior, overwrite=TRUE)

nCRMPosteriorShort <-
  fitModel(
    getModelString("crm"),
    nCRMPatientData %>% dplyr::select(XHat, Toxicity),
    inits=list(
      list(theta=c(1,log(4))),
      list(theta=c(0,log(1))),
      list(theta=c(-2,log(7)))
    ),
    extras=list(
      "precision"=solve(matrix(c(1, -0.1, -0.1, 1), ncol=2)),
      "mean"=c(0, 1)),
    sample=2000,
    thin=2
  )
usethis::use_data(nCRMPosteriorShort, overwrite=TRUE)

