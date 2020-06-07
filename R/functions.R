#' Create Summary Statistics from a MCMC Samples
#'
#' Given a tibble equivalent of a \code{mcmc} or \code{mcmc.list} object,
#' creates a tibble of summary statistics by dose.
#'
#' @param data the mcmc tibble to be summarised
#' @param dose the name of the column in \code{data} which defines the dose
#' @param var the name(s) of the variable(s) to be summarised.  See Usage notes
#' below.
#' @param summaryStats the statistics to be produced.  See Usage notes below.
#' @param quantiles the quantiles to be produced.  Convenience parameter to
#'  avoid having to respecify the whole of \code{summaryStats} just to get
#'  different quantiles.
#' @param normalise Should the return value be normalised?  See Usage notes
#' below.
#' @return a tibble containing the requested summary statistics.  See Usage
#'  notes below.
#' @section Usage notes:
#' The \code{summaryStats} argument provides flexibility at the cost of
#' complexity.  However, the default value, which returns the mean, standard
#' deviation, and arbitrary quantiles, should be sufficient for most purposes.
#' The \code{quantiles} argument allows arbitrary quantiles to be specified
#' without the need to respecify  \code{summaryStats} completely.
#'
#' By default, the returned tibble contains three columns: Dose, Statistic and
#' Value, with the obvious interpretations.  If \code{normalise=FALSE}, the
#' tibble contains the column Dose, together with one column for each of the
#' summary statistics requested.  The names of the columns is either the name
#' of the function which produces the statistic, or as specified by the name of
#' the corresponding element in the \code{summaryStats} argument.  If a summary
#' function returns more than one element, the corresponding column is nested.
#' For example, with \code{normalise=FALSE} and \code{summaryStats} at its
#' default value, the returned value contains columns named Dose, Mean, SD and
#' Q.  Each value of Q is itself a tibble with two columns (Quantile and Value)
#' and five rows.
#'
#' More than one variable can be specified in \code{var}.  For example,
#' \code{var=Prob1:Prob2}.  In this case, each column (apart from Dose) in the
#' returned tibble will be nested, and the returned value most likely need
#' post-processing, as \code{normalise=TRUE} will almost certainly fail.
#'
#' If more than one of the requested summary statistics return multiple values
#' AND the number of values returned differs between summary statistics,
#' \code{normalise=TRUE} will fail:  set \code{normalise=FALSE} and post-process
#' the returned value in a custom fashion.
#' @examples
#' #Generate some test data
#' x <- runjagsToTibble(generateOCRMData(chainLength=1000)[["posterior"]])
#' x <- augmentOCRMData(x)
#' #Default output
#' createDoseSummary(x, var=Prob2)
#' #Unnormalised output
#' createDoseSummary(x, var=Prob2, normalise=FALSE)
#' #Default output, spread for presentation
#' createDoseSummary(x, var=Prob2) %>% tidyr::spread(key=Statistic, value=Value)
#' #Multiple variables
#' createDoseSummary(x, var=Prob1:Prob2)
#' @export
createDoseSummary <-function(data,
                             dose=Dose,
                             var=Prob,
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
                             normalise=TRUE) {
  d_ <- dplyr::enquo(dose)
  v_ <- dplyr::enquo(var)

  rv <- data %>%
    dplyr::group_by(!! d_) %>%
    dplyr::summarise_at(.vars=dplyr::vars(!! v_),
                        .funs=summaryStats
    )
  if (normalise) {
    hasQuantiles <-"Q" %in% names(rv)
    if (hasQuantiles) {
      rv <- rv %>%
        tidyr::unnest() %>%
        dplyr::mutate(Temp=paste0("Q", Quantile)) %>%
        dplyr::select(-.data$Quantile) %>%
        dplyr::rename(Quantile=Temp) %>%
        tidyr::spread(key=Quantile, value=.data$Value)
    }
    rv <- rv %>%
      tidyr::gather(key="Statistic", value="Value", -!! d_)
  }
  return (rv)
}

#' Convert MCMC Sample Chains produced by \code{runjags} to a Tibble
#'
#' If the class of \code{obj} is \code{runjags}, the \code{mcmc} field is
#' extracted and processed.  If \code{runjags.list} or \code{mcmc.list},
#' \code{runjagsToTibble} calls itself recursively on each element of
#' \code{obj}, \code{row_bind()}s the results together and returns the
#' resulting tibble.  If \code{mcmc}, \code{obj} is processed as described in
#' Usage notes below.
#'
#' @param obj the object to be converted.  See Usage notes below.
#' @param chain the chain id
#' @param xhatFunc If not \code{NULL}, the function which augments the
#' converted \code{obj}
#' with columns that are relevant to future processing.  See Usage notes below.
#' @param indexedPattern the regular expression used to identify indexed model
#' parameters.  Ensure that the pattern includes groups to return the "array"
#' name and the index.  The default value isn't perfect - it allows, for
#' example, _3a to be a valid variable name - but it sould be good enough for
#' most purposes.
#' @return the corresponding tibble.  See Usage notes below.
#' @section Usage notes:
#' Typically, the CRM model parameters are of little intrest in their own right.
#' The probabiities derived from them are more relevant.  If the JAGS code
#' derives these probabilities, they will be returned in the tibble as described
#' below.  If not, the \code{xhatFunc} argument provides a hook to allow them
#' to be derived.  \code{xhatFunc} is called immediately before the transformed
#' obj is returned, so it must process obj in its transformed state.
#' \code{xhatFunc} should take a single argument: the transformed object.  The
#' transformed object is in normal form, which means that usually it must be
#' \code{spread()} so that all model parameters are available in a single row.
#' This is awkward, but takes little time.  See the vignette for an example.
#'
#' Assuming that \code{xhatFunc} is \code{NULL}, the tibble returned by this
#' function contains the following columns:
#' \code{Chain}: an integer containing the index of the corresponding
#' \code{mcmc} object in the input \code{mcmc.list}.  If the input was an
#' \code{mcmc} object, \code{Chain} is \code{1} for all rows.
#' \code{Sample}: the sample number, from the input\code{mcmc} object.
#' \code{Parameter}: the name of the column in the input \code{mcmc} object to
#' which \code{value} relates.  If the JAGS model parameter is indexed (for
#' example, \code{alpha[1]}), \code{Parameter} contains the array name (here,
#' \code{alpha}) and \code{Index} contains the array index (here, \code{1}).
#' \code{Index}: if the corresponding \code{Parameter} is indexed, the value of
#' the index.  Otherwise, \code{NA}.
#' \code{Value}: The value of this \code{Parameter} drawn in this \code{Sample}
#' of this \code{Chain}.
#'
#' If \code{xhatFunc} is not \code{NULL}, the return value depends on the
#' actions of \code{xhatFunc}.
#' @examples
#' x <- generateNCRMData()
#' runjagsToTibble(x[["posterior"]])
#' @importFrom rlang .data
#' @export
runjagsToTibble <- function(obj,
                            chain=1,
                            xhatFunc=NULL,
                            indexedPattern="([:alnum:\\.]*)\\[(\\d*)\\]")
{
  if (is.null(obj)) {
    warning("Object is null or NA.  Nothing to do.  Returning NA...")
    return(NA)
  }
  if ("runjags" %in% class(obj)) {
    return(runjagsToTibble(obj$mcmc, chain, xhatFunc, indexedPattern))
  }
  if ("runjags.list" %in% class(obj)) {
    return (dplyr::bind_rows(lapply(1:length(obj),
                                    function(i) runjagsToTibble(obj[[i]], i, xhatFunc, indexedPattern)
                                    )
                             )
            )
  }
  if ("mcmc.list" %in% class(obj)) {
    return (dplyr::bind_rows(lapply(1:length(obj),
                                    function(i) runjagsToTibble(obj[[i]], i, xhatFunc, indexedPattern)
                                    )
                             )
            )
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
          tidyr::separate(col=Temp,
                          into=c("Parameter", "Index", NA),
                          sep="([\\[\\]])",
                          convert=TRUE,
                          fill="right") %>%
          dplyr::arrange(Chain, Sample, Parameter, Index)
  if (!is.null(xhatFunc)) {
    rv <- xhatFunc(rv)
  }
  return (rv)
}

#Why oh why oh why????
# temp <- generateOCRMData(n=1000)
# temp1 <- runjagsToTibble(temp[["posterior"]], xhatFunc=augmentOCRMData)
#
# temp2 <- runjagsToTibble(temp[["posterior"]])
# temp2 <- augmentOCRMData(temp2)


#' Graph of Dose Adminsitered vs Subject Number
#'
#' The graph indicates toxicity status using colur and, optionally, indicates
#' cohort boundaries and the MTD at the end of the trial
#' @param d a tibble containing subject numbers, dose alloactions and
#' toxicity status.  Usually of class "ocrmSubjects" or "crmSubjects".
#' @param x the column in \code{d} defining the subject ID
#' @param y the column in \code{d} defining the dose
#' @param cohort the column in \code{d} defining the cohort
#' @param tox the column in \code{d} defining the toxicity status
#' @param toxColours the colours used to identify the DLT status of each
#'  subject.  See Usage notes below.
#' @param toxLabels the texts used to label the various DLT statuses in the
#' legend.  See Usage notes below.
#' @param showCohorts  should the graph indentify cohorts by inserting light
#' grey dashed lines between cohorts?
#' @param xLabel the text used to label the x axis
#' @param yLabel the text used to label the y axis
#' @param mtd the dose identified as the MTD at the end of the study.  Appears
#' in green at (maxSubject + 0.25, mtd)  where maxSubject is the maximum
#' subject number and mtd is the dose identified as the MTD.
#' @param type Optional flag to indicate the type of the trial being plotted.
#' Supported values are "crm" and "ocrm".  See Usage notes below.
#' @param ... Not currently used
#' @return the ggplot2 object containing the plot.
#' @section Usage notes:
#' If \code{toxLabels} and/or \code{toxColours} are specfied, the values
#'  supplied are used.  Otherwise, the value of \code{type} is used to specify
#'  the number of toxicity categories and the colours used to represent them.
#'  If \code{type == "crm"}, then  \code{toxLabels = c("None", "DLT")} and
#'  \code{toxColours=c("grey", "red")}.  If \code{type == "ocrm"}, then
#'  \code{toxLabels = c("None", "Sub-DLT", DLT")} and
#'  \code{toxColours=c("grey", "gold", red")}.
#' @examples
#' x <- generateOCRMData(chainLength=1000)[["patients"]]
#' subjectGraph(x, type="ocrm")
#' @export
subjectGraph <- function(d,
                         x=SubjectID,
                         y=Dose,
                         cohort=Cohort,
                         tox=Toxicity,
                         toxColours=NA,
                         toxLabels=NA,
                         showCohorts=TRUE,
                         xLabel="Subject",
                         yLabel="Dose",
                         mtd=NA,
                         type=c("crm", "ocrm"),
                         ...) {

  #Set up
  y_ <- dplyr::enquo(y)
  x_ <- dplyr::enquo(x)
  c_ <- dplyr::enquo(cohort)
  tox_ <- dplyr::enquo(tox)

  type <- match.arg(type)
  if (is.na(toxColours)) {
    if (type == "crm") {
      toxColours <- c("grey", "red")
    } else {
      toxColours <- c("grey", "gold", "red")
    }
  }
  if(is.na(toxLabels)) {
    if (type == "crm") {
      toxLabels <-  c("None", "DLT")
    } else {
      toxLabels <- c("None", "Sub-DLT", "DLT")
    }
  }

  #Begin
  plot <- d %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x=!! x_, y=!! y_, colour=as.factor(!! tox_))) +
    ggplot2::scale_colour_manual(values=toxColours,
                                 labels=toxLabels,
                                 name="Toxicity") +
    ggplot2::theme_light() +
    ggplot2::labs(x=xLabel,
                  y=yLabel)
  if (showCohorts) {
    cLines <- d %>%
      dplyr::group_by(!! c_) %>%
      dplyr::summarise(X=max(!! x_) + 0.5) %>%
      dplyr::ungroup()
    cLines <- cLines %>% utils::head(nrow(cLines)-1)
    plot <- plot +
      ggplot2::geom_vline(data=cLines, ggplot2::aes(xintercept=X), linetype="dashed", colour="lightgrey")
  }
  if (!is.na(mtd)) {
    plot <- plot +
      ggplot2::geom_point(data=NULL, ggplot2::aes(x=max(d$SubjectID)+0.25, y=mtd, shape="MTD"), colour="green") +
      ggplot2::guides(shape=ggplot2::guide_legend(title=ggplot2::element_blank()))
  }

  return(plot)
}

#' Produce a Dose-Toxicity Curve
#'
#' Produce a dose-toxicity curve with posterior credible intervals for the
#' probability of toxicity at each dose.  Optionally, display the target toxicity
#' rate (for CRM and oCRM studies) or the target toxicity band (for nCRM
#' studies).
#' @param d the input tibble, created by \code{createDoseSummary}, which
#' must be normalised
#' @param dose the column in \code{d} that defines the dose
#' @param lines a vector of \code{Parameter}s in \code{d} whose \code{Value}s
#' are to be represented as lines in the graph.  See Usage notes below.
#' @param ribbons a list of sets of (pairs of) \code{Parameter}s whose
#' \code{Value}s define low and high bounds of ribbons on the plot.  Each list
#' element defines a separate set of ribbons.  Each entry (of two
#' \code{Parameter} values) within each list element defines a seperate ribbon
#' within a set.  By default, \code{Parameter}s that define quantiles undergo
#' special processing.  See Usage notes below.
#' @param quantiles the numeric quantiles represented by the quantile
#' \code{Parameter} values
#' @param colours a named vector defining the colours to be used in the various
#' plot elements.  the names define the plot elemets, the values define the
#' colour to be used in the corresponding element
#' @param yLabel the text to be used to label the y axis
#' @param xLabel the text to be used to label the x axis
#' @param labels a named list of legend and category labels.  The names define
#' the legend titles and elements.  The values define the text to label the
#' corresponding element.  See Usage notes below.
#' @param targetTox a scalar or two element vector defining the target toxicity
#' rate (if scalar) or band (if a vector).
#' @param alphaRange a two element vector defining the upper and lower bounds
#' for the alpha (1 minus the transparency) level used to paint all ribbons in
#' the graph.
#' @param defaultLegends if \code{TRUE} apply default legends to the various
#' graph elements.  If \code{FALSE} no legends are defined, the value of
#' code{labels} is ignored and the resulting plot will almost certainly need
#' custom post-processing to be useful
#' @return a ggplot object containing the requested plot
#' @examples
#' x <- runjagsToTibble(generateOCRMData(chainLength=1000)[["posterior"]])
#' x <- augmentOCRMData(x)
#' y <- createDoseSummary(x, var=Prob1) %>% dplyr::filter(Statistic != "SD")
#' doseToxicityGraph(y, targetTox=c(0.15, 0.30))
#' @section Usage notes:
#' TODO: Fix the colour of legend entries for ribbons.  Their transparency is correct.
#' TODO: Allow custom colour for the target toxicity rate/band
#' TODO: Allow wild card specification for all ribbon specifications.
#'
#' The input tibble is \code{spread} so that \code{Parameter} values become
#' column names for use in the plot.
#'
#' The definition of ribbons is complex at first sight, but highly flexible.  If
#' \code{ribbons=list("ribbonA"=list(c("Stat1", "Stat2"), c(("Stat3", "Stat4"))))}
#' then a single set of two ribbons will be produced.  The first ribbon is
#' runs from \code{Value}s corresponding to \code{Parameter == "Stat1"}
#' (the lower limit of the ribbon) to \code{Value}s corresponding to
#' \code{Parameter == "Stat2"} (the upper limit of the ribbon).  The definition
#' of the second ribbon is similar.  The ribbons are drawn in the order they
#' appear in the list and so there is an assumption that the ribbons get
#' narrower as the length of the list increases.  In other words, after the input
#' tibble has been \code{spread}, that Stat1 < Stat2 < Stat3 < Stat4.
#'
#' Continuing the example above, if \code{labels=c("ribbonA"="My ribbon label", "ribbonA1"="First category", "ribbonA2"="Second label")}
#' then the corresponding legend has a title of "My ribbon label" and its two
#' entries are labelled "First category" and "Second category".
#'
#' Quantile ribbons undergo special pre-processing.  By default, quantiles are
#' flagged by \code{Parameter} values of the form "Q<x>", where <x> is a
#' positive integer.  First, if the \code{quantiles} list contains the value 0.5
#' in its ith element, then \code{Parameter} values of "Q<i>" are changed to
#' "Median".  Then the remaining "Q<x>" values are paired.  If there are n
#' remaining elements, the pairs are ("Q<1>", Q<n>"), ("Q<2>", Q<n-1>") and
#' so on.  These pairs are then processed as described above.
#' @export
doseToxicityGraph <- function(d,
                              dose=Dose,
                              lines=c("Mean", "Median"),
                              ribbons=list("quantile"="Q\\d*"),
                              quantiles=c(0.05, 0.10, 0.50, 0.90, 0.95),
                              colours=c("mean"="black", "median"="blue", "quantile"="steelblue"),
                              yLabel="p(Toxicity)",
                              xLabel="Dose",
                              labels=c("quantile"="Posterior CIs", "lines"="Dose-tox curve", "quantile1"="10%-90%", "quantile2"="20%-80%"),
                              targetTox=0.33,
                              alphaRange=c(0.2, 0.4),
                              defaultLegends=TRUE) {
  #Validate
  if (!methods::is(d, "tbl")) stop("d must be a tibble.")
  if (isFALSE(all.equal(colnames(d), c("Dose", "Statistic", "Value")))) stop("d must be normalised")

  d_ <- dplyr::enquo(dose)

  #Rename the variable containing the 50th centile to (by default) Median, if
  #necessary
  medIdx <- which(quantiles == 0.5)
  if (medIdx > 0) {
    medVar <- paste0("Q", medIdx)
    d <- d %>% dplyr::mutate(Statistic=ifelse(.data$Statistic == medVar, "Median", .data$Statistic))
  }

  #Special handling of quantile specification
  if ("quantile" %in% names(ribbons)) {
    quantNames <- (d %>%
                     dplyr::filter(grepl(ribbons[["quantile"]], .data$Statistic)) %>%
                     dplyr::distinct(.data$Statistic) %>%
                     dplyr::select(.data$Statistic))$Statistic

    nQuantVars <- length(quantNames)
    if (nQuantVars %% 2 == 0) {
      lim <- nQuantVars / 2
      q <- vector("list", length=lim)
      for (i in 1:lim) {
        e <- vector("numeric", length=2)
        e[1] <- quantNames[i]
        e[2] <- quantNames[nQuantVars - i + 1]
        q[[i]] <- e
      }
      ribbons[["quantile"]] <- q
    } else {
      warning(paste0("Number of quantile variables (after removing the median, if necessary) [", nrow(quantNames), "] is odd.  The ribbon request for quantiles will be ignored."))
      ribbons[["quantile"]] <- NULL
    }
  }

  #Process targetTox request
  if (!is.null(targetTox & !is.na(targetTox))) {
    if (rlang::is_scalar_atomic(targetTox)) {
      toxRequest <- ggplot2::geom_hline(data=NULL, ggplot2::aes(yintercept=targetTox), colour="green")
    } else {
      if (length(targetTox) > 2) warning("More than two elements in targetTox: only the first two will be used.")
      toxRequest <- ggplot2::geom_ribbon(data=d, ggplot2::aes(x=Dose, ymin=targetTox[1], ymax=targetTox[2]), alpha=0.25, fill="green")
    }
  } else {
    toxRequest <- NULL
  }

  #Process line requests
  lineData <- d %>%
    dplyr::filter(.data$Statistic %in% lines) %>%
    tibble::add_column(Quantile=0)
  lineRequests <- ggplot2::geom_line(data=lineData,
                                     ggplot2::aes(x=!! d_, y=.data$Value, colour=.data$Statistic))

  #Process ribbon requests
  ribbonNames <- names(ribbons)
  ribbonData <- vector("list", length=length(ribbons))
  for (i in 1:length(ribbons)) {
    r <- ribbons[[i]]
    rData <- vector("list", length=length(r))
    for (j in 1:length(r)) {
      r1 <- r[[j]]
      alphaList <- seq(from=alphaRange[1], to=alphaRange[2], length.out=length(r))
      rData[[j]] <- d %>%
        dplyr::filter(.data$Statistic %in% r1) %>%
        tidyr::spread(key=.data$Statistic, value=.data$Value)
      rData[[j]] <- rData[[j]] %>%
        dplyr::rename(Min=r1[1], Max=r1[2]) %>%
        tibble::add_column(Group=ribbonNames[i],
                           Ribbon=alphaList[j],
                           Colour=colours[Group])
    }
    ribbonData[[i]] <- dplyr::bind_rows(rData)
  }
  ribbonData <- dplyr::bind_rows(ribbonData)

  ribbonRequests <- ggplot2::geom_ribbon(data=ribbonData,
                                         ggplot2::aes(x=Dose,
                                                      ymin=Min,
                                                      ymax=Max,
                                                      group=Ribbon,
                                                      fill=Group,
                                                      alpha=Ribbon),
                                         fill=ribbonData$Colour)

  #Build the plot.  (Do it this way to control layer order.)
  plot <- ggplot2::ggplot()
  if (!is.null(toxRequest)) plot <- plot + toxRequest
  if (!is.null(ribbonRequests)) plot <- plot + ribbonRequests
  if (!is.null(lineRequests)) plot <- plot + lineRequests


  plot <- plot +
    ggplot2::theme_light() +
    ggplot2::labs(x=xLabel,
                  y=yLabel)

  if (defaultLegends) {
    lineColours <- colours[tolower(lines)]
    names(lineColours) <- NULL
    plot <- plot +
      ggplot2::scale_colour_manual(name=ifelse(is.null(labels["lines"]), " ", labels["lines"]),
                                   labels=lines,
                                   values=lineColours)
    if (!is.null(ribbons)) {
      for(i in length(ribbons)) {
        lbl <- ifelse(is.null(labels[ribbonNames[i]]), ribbonNames[i], labels[ribbonNames[i]])
        catLabels <- unname(labels[grepl(paste0(ribbonNames[i], "\\d+"), names(labels))])
        #names(catLabels) <- NULL
        breakList <- seq(from=alphaRange[1], to=alphaRange[2], length.out=2)
        suppressWarnings(plot <- plot +
                           ggplot2::scale_alpha(name=labels[ribbonNames[i]],
                                                range=alphaRange,
                                                breaks=breakList,
                                                labels=catLabels

                                                #TODO: allow legend to pick up colour of fill, not just the transparency
                                                # ,
                                                # palette=function(x) {
                                                #   return (rgb(red=50, green=130, blue=180, alpha=ceiling(255*c(0.2, 0.4)), maxColorValue=255, names=c("quantile1", "quantile2")))
                                                #   return (c(0.2, 0.4))
                                                # }
                           )
        )
      }
    }
  }

  return (plot)
}
