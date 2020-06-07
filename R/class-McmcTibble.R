#' @importFrom tibble tibble
#' @importFrom methods new
.McmcTibble <- setClass("McmcTibble",
                         slots=c("data"="tbl",
                                 "columnLabels"="vector",
                                 "modelString"="character",
                                 "chainCount"="numeric",
                                 "chainLength"="numeric",
                                 "thin"="numeric",
                                 "augmented"="logical"),
                         validity=function(object){
                           errors <- character()
                           if (!is.na(object@chainLength) & object@chainLength <= 0) {
                             errors <- c(errors, "chainLength must be strictly positive")
                           }
                           if (!is.na(object@chainCount) & object@chainCount <= 0) {
                             errors <- c(errors, "chainCount must be strictly positive")
                           }
                           if (!is.na(object@thin) & object@thin <= 0) {
                             errors <- c(errors, "thin must be strictly positive")
                           }
                           if (length(errors) == 0) TRUE else errors
                         })

setMethod("initialize",
          "McmcTibble",
           function (.Object, ...) {
             args <- list(...)
             .Object <- callNextMethod()
             .Object@chainCount <- ifelse(is.na(args[["chainCount"]]), NA, args[["chainCount"]])
             .Object@chainLength <- ifelse(is.na(args[["chainLength"]]), NA, args[["chainLength"]])
             .Object@thin <- ifelse(is.na(args[["thin"]]), NA, args[["thin"]])
             .Object@modelString <- ifelse(is.na(args[["modelString"]]), "", args[["modelString"]])
             .Object@augmented <- FALSE
             validObject(.Object)
             .Object
           })

#validObject(.McmcTibble())

McmcTibble <- function(obj, augment=NULL, ...)
{
  mcmcToTibble <- function(obj,
                           chain=1,
                           indexedPattern="([:alnum:\\.]*)\\[(\\d*)\\]") {
    if ("runjags" %in% class(obj)) {
      return(mcmcToTibble(obj$mcmc))
    }
    if ("runjags.list" %in% class(obj)) {
      return (dplyr::bind_rows(lapply(1:length(obj),
                                      function(i) mcmcToTibble(obj[[i]], i))))
    }
    if ("mcmc.list" %in% class(obj)) {
      return (dplyr::bind_rows(lapply(1:length(obj),
                                      function(i) mcmcToTibble(obj[[i]], i))))
    }
    if (!("mcmc.list" %in% class(obj)) & !("mcmc" %in% class(obj))) {
      stop("obj must have class mcmc, mcmc.list, runjags or runjags.list")
    }

    a <- attributes(obj)
    colNames <- a$dimnames[[2]]
    #Get the sample ids and add the chain index
    base <- tibble::enframe(a$dimnames[[1]], value="Sample") %>%
      tibble::add_column(Chain=chain, .before=1) %>%
      dplyr::select(-.data$name)

    rv <- base %>% dplyr::filter(1 == 0)
    for (i in 1:length(colNames)) {
      t <- base %>%
             tibble::add_column(Temp=colNames[i]) %>%
             tibble::add_column(Value=obj[,colNames[i]])
      #Known "feature" of bind_rows
      suppressWarnings(rv <- rv %>%  dplyr::bind_rows(t))
    }
    rv <- rv %>%
      tidyr::separate(col=.data$Temp,
                      into=c("Parameter", "Index", NA),
                      sep="([\\[\\]])",
                      convert=TRUE,
                      fill="right") %>%
      dplyr::arrange(.data$Chain, .data$Sample, .data$Parameter, .data$Index) %>%
      dplyr::mutate(Sample=as.integer(.data$Sample))
    return (rv)
  }

  d <- mcmcToTibble(obj)
  #Populate secondary slots
  if (methods::is(obj, "runjags") | methods::is(obj, "runjags.list")) {
    chainCount <- length(obj$mcmc)
    chainLength <- as.integer(obj$sample)
    thin <- obj$thin
    modelString <- utils::capture.output(obj$model)
  } else {
    if (methods::is(obj, "mcmc.list")) {
      x <- obj[[1]]
    } else {
      x <- obj
    }
    chainCount <- length(x)
    z <- attributes(x)$mcpar
    chainLength <- as.integer((z[2] - z[1] + z[3]) / z[3])
    thin <- z[3]
    modelString <- "Unknown"
  }
  rv <- .McmcTibble(data=d,
                    columnLabels=names(d),
                    chainLength=chainLength,
                    chainCount=chainCount,
                    thin=thin,
                    modelString=modelString)
  if (is.function(augment)) {
    rv@data <- augment(rv@data, ...)
    rv@columnLabels <- names(rv@data)
    rv@augmented <- TRUE
  }
  validObject(rv)
  return(rv)
}

setGeneric("augment", valueClass="McmcTibble", function(d, f, doseGrid, columnLabels) {
  standardGeneric("augment")
})

setMethod("augment",
          signature(d="McmcTibble", f="function", doseGrid="numeric", columnLabels="character"),
          function(d, f, doseGrid, columnLabels=NULL) {
            d@data <- f(d@data, doseGrid)
            #columnLabels <- names(d@data)
            d@columnLabels <- columnLabels
            d@augmented <- TRUE
            if (ncol(d@data) != length(columnLabels))
              warning(paste0("Length of columnLabels [", length(columnLabels), "] does not match number of columns [", ncol(d@data), "]."))
            return(d)
          })

setMethod("augment",
          signature(d="McmcTibble", f="function", doseGrid="numeric", columnLabels="missing"),
          function(d, f, doseGrid, columnLabels) {
            d@data <- f(d@data, doseGrid)
            d@columnLabels <- names(d@data)
            d@augmented <- TRUE
            return(d)
          })

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
    tidyr::gather(key=Parameter, value=Value, Prob1, Prob2) %>%
    dplyr::ungroup()
  return(d)
}

ocrm <- generateOCRMData(chainLength=5000)
x <- ocrm[["posterior"]]
patData <- ocrm[["patients"]]
y <- McmcTibble(x)
y1 <- augment(y, augmentOCRMData, 1:10)
z <- McmcTibble(x$mcmc)
z1 <- augment(z, augmentOCRMData, 1:10)
z1
w <- McmcTibble(x, augment=augmentOCRMData, doseGrid=1:10)
w
