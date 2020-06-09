.DoseSummary <- setClass("DoseSummary",
                         slots=c("summary"="tbl",
                                 "columnLabels"="vector",
                                 "hasQuantiles"="logical",
                                 "normalised"="logical",
                                 "quantiles"="numeric",
                                 "footnotes"="list"),
                         validity=function(object){
                           return (TRUE)
                         })

DoseSummary <- function(data,
                        dose=Dose,
                        var=Value,
                        parameter=Parameter,
                        summaryStats=dplyr::funs("Mean"=mean(., na.rm=TRUE),
                                                 "SD"=stats::sd(., na.rm=TRUE),
                                                 "Q" = list(tibble::enframe(stats::quantile(.,
                                                                                            probs = quantiles,
                                                                                            na.rm=TRUE,
                                                                                            names=FALSE
                                                 ),
                                                 name="Quantile",
                                                 value="Value"
                                                 )
                                                 )
                        ),
                        quantiles=c(0.05, 0.10, 0.50, 0.90, 0.95),
                        normalise=TRUE,
                        columnLabels=NULL,
                        ...) {
  #Validate
  if (!methods::is(data, "McmcTibble")) stop("data is not an McmcTibble")
  if (!data@augmented) stop("data is not augmented")
  #Initialise
  d_ <- dplyr::enquo(dose)
  v_ <- dplyr::enquo(var)
  p_ <- dplyr::enquo(parameter)

  #Begin
  temp <- data@data %>%
    dplyr::group_by(!! p_, !! d_) %>%
    dplyr::summarise_at(.vars=dplyr::vars(!! v_),
                        .funs=summaryStats) %>%
    dplyr::ungroup()
  #Unnest quantiles
  hasQuantiles <- "Q" %in% names(temp)
  if (hasQuantiles) {
    temp <- temp %>%
      tidyr::unnest() %>%
      dplyr::mutate(Temp=paste0("Q", Quantile)) %>%
      dplyr::select(-.data$Quantile) %>%
      dplyr::rename(Quantile=Temp) %>%
      tidyr::spread(key=Quantile, value=.data$Value)
  }
  if (normalise) {
    temp <- temp %>%
              tidyr::gather(key="Statistic",
                            value="Value",
                            -!! d_, -!! p_) %>%
              dplyr::select(Dose, Parameter, Statistic, Value)
  }
  if (is.null(columnLabels)) {
    if (hasQuantiles) {
      columnLabels <- names(temp)
      columnLabels <- modifyQuantileLabels(columnLabels, quantiles)
    } else {
      columnLabels <- names(temp)
    }
  }
  fNotes <- list(sprintf("Based on %d chain%s of length %d.", data@chainCount, ifelse(data@chainCount == 1, "", "s"), data@chainLength))
  rv <- .DoseSummary(summary=temp,
                     columnLabels=ifelse(is.null(columnLabels), names(temp), columnLabels),
                     normalised=normalise,
                     hasQuantiles=hasQuantiles,
                     quantiles=quantiles,
                     footnotes=fNotes)
  return (rv)
}


setGeneric("render", function(data, type, ...) {
  standardGeneric("render")
})

setMethod("render",
          signature(data="DoseSummary", type="character"),
          function(data, type=c("knitr", "officer"), ...) {
            type <- match.arg(type)
            if (type == "knitr") {
              return (renderKnitr(data, ...))
            } else if (type == "officer") {
              return (renderOfficer(data, ...))
            } else stop (paste0(type, " is not a supported destination"))
          })


setMethod("render",
          signature(data="DoseSummary", type="missing"),
          function(data, type=c("knitr", "officer"), ...) {
            if (requireNamespace("knitr", quietly=TRUE)) print("I could use knitr...")
            else print("knitr is not available")
            if (requireNamespace("officer", quietly=TRUE)) print("I could use officer...")
            else print("officer is not available")
          })

setGeneric("renderKnitr", function(data, ...) {
  standardGeneric("renderKnitr")
})

setMethod("renderKnitr",
          signature(data="DoseSummary"),
          function(data,
                   patientSummary=NULL,
                   parameter=Parameter,
                   dose=Dose,
                   statistic=Statistic,
                   value=Value,
                   columnLabels=NULL,
                   spread=TRUE,
                   digits=3,
                   hook=NA,
                   idColumns=NULL,
                   showFootnotes=TRUE,
                   format=c("html", "latex", "markdown", "pandoc", "rst")) {
            #Initialise
            p_ <- dplyr::enquo(parameter)
            d_ <- dplyr::enquo(dose)
            s_ <- dplyr::enquo(statistic)
            v_ <- dplyr::enquo(value)
            format <- match.arg(format)

            #Begin
            if (spread & data@normalised) {
              d <- data@summary %>%
                     tidyr::spread(key=!! s_, value=!! v_) %>%
                     dplyr::arrange(!! d_, !! p_)
              if (is.null(columnLabels))
                modifyQuantileLabels(names(d), data@quantiles)
            } else {
              d <- data@summary
              if (is.null(columnLabels)) columnLabels <- data@columnLabels
            }
            #Merge with patient data, if necessary
            if (!is.null(patientSummary)) {
              d <- patientSummary@summary %>% dplyr::full_join(d, by=dplyr::quo_name(d_))
            }
            #Post process summary data
            if (is.function(hook)) d <- hook(d)
            #Final tweaks of column labels
            if (is.null(columnLabels)) {
              columnLabels <- names(d)
              if (!is.null(patientSummary)) columnLabels[1:length(patientSummary@columnLabels)] <- patientSummary@columnLabels
              if (!is.null(data@quantiles))
                columnLabels <- modifyQuantileLabels(columnLabels, data@quantiles)
            }
            #Double check column labels
            if (length(names(d)) != length(columnLabels)) {
              warning(paste0("Length of columnLabels [",
                             length(columnLabels),
                             "] is not equal to the number of columns in the table [",
                             length(names(d)),
                             "]. columnLabels will be ignored."))
              warning("(This is most likely to occur when the hook function alters the number of columns in the table.")
              columnLabels <- names(d)
              if (!is.null(data@quantiles))
                columnLabels <- modifyQuantileLabels(columnLabels, data@quantiles)
            }
            table <- d %>%
                       knitr::kable(format=format,
                                    col.names=columnLabels,
                                    digits=digits)
            if (requireNamespace("kableExtra", quietly=TRUE)) {
              #Column grouping must be handled before footnotes are added.
              if (!is.null(idColumns)) {
                table <- table %>%  kableExtra::collapse_rows(columns=idColumns, valign="top")
              }
              if (length(data@footnotes) > 0 & showFootnotes) {
                 table <- table %>% kableExtra::footnote(general=data@footnotes[[1]])
              }
              if (data@hasQuantiles & spread) {
                qList <- which(!is.na(stringr::str_match(names(d), "Q\\d+"))[,1], arr.ind=TRUE)
                spanHeader <- list()
                if(!is.null(patientSummary)) {
                  qStart <- ncol(patientSummary@summary)
                  spanHeader <- append(spanHeader, c(" "=1))
                  spanHeader <- append(spanHeader, c("Subjects"=qStart-1))
                } else qStart <- 1
                if (min(qList)  > qStart) spanHeader <- append(spanHeader, list(" "=min(qList)-qStart-1))
                spanHeader <- append(spanHeader, list("Quantiles"=max(qList) - min(qList) + 1))
                if (ncol(d) > max(qList)) spanHeader <- append(spanHeader, list(" "=ncol(d) - max(qList)))
                spanHeader <- unlist(spanHeader)
                print(spanHeader)
                table <- table %>% kableExtra::add_header_above(spanHeader)
              }
            } else {
              warning("Package kableExtra is not available: unable to finalise table formatting")
            }
            return(table)
          })

setGeneric("renderOfficer", function(data, ...) {
  standardGeneric("renderOfficer")
})

setMethod("renderOfficer",
          signature(data="DoseSummary"),
          function(data, ...) {
            stop("Not yet implemented")
          })
