#' Plot the posterior p(Tox) by dose
#' @param data The tibble containing subject level dose allocations and toxicity status
#' @param dose the name of the column in \code{data} which defines the dose
#' @param value the name of the column in \code{data} which defines p(Tox) for this sample
#' @param recDose optional.  Identifies the currently recommended dose.  If not \code{NA}
#' the PDF for the given dose is drawn in  1 colour (default blue), all other doses in
#' another (default grey).  Otherwise, each dose is individually identified by colour
#' @param targetTox optional.  If not \code{NULL}, the target toxicity dose (if a scalar)
#'  or the target toxicity band (if a vector of length 2) is highlighted in green.
#' @examples
#'data("oQuigleyPosterior")
#' x <- oQuigleyPosterior %>%
#'        tidy() %>%
#'        augmentOQuigleyData() %>%
#'        dplyr::filter(Parameter == "Prob")
#' posteriorPlot(x, targetTox=c(0.2, 0.33), recDose=2)
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_colour_discrete
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 element_blank
#' @export
posteriorPlot <- function(
  data,
  dose=Dose,
  value=Value,
  recDose=NA,
  targetTox=NULL
) {
  d <- data
  if (!is.na(recDose)) d <- d %>% mutate(RecDoseFlag=Dose == recDose)
  p <- d %>% ggplot()
  if (is.na(recDose)) {
    p <- p +
      geom_density(aes(x=Value, colour=as.factor(Dose))) +
      scale_colour_discrete(name="Dose")
  } else {
    p <- p +
      geom_density(aes(x=Value, colour=RecDoseFlag, group=as.factor(Dose))) +
      scale_colour_manual(values=c("grey", "blue"), labels=c("Other", "Rec dose"), name="Dose")
  }
  p <- p +
    labs(x="P(Tox)") +
    theme_light() +
    theme(
      axis.ticks.y=element_blank(),
      axis.text.y=element_blank(),
      axis.title.y=element_blank()
    )
  if (!is.null(targetTox)) {
    if (length(targetTox) > 1) {
      yMax <- ggplot_build(p)$layout$get_scales(1)$y$range$range[2]
      p <- p + geom_area(data=tibble(), aes(x=targetTox, y=c(yMax, yMax)), fill="green", alpha=0.35, show.legend=FALSE)
    } else {
      p <- p + geom_vline(aes(xintercept=targetTox), colour="green", show.legend=FALSE)
    }
  }
  return(p)
}

#' Plot subject allocations
#'
#' @param subjectData The tibble containing subject level dose allocations and toxicity status
#' @param dose the name of the column in \code{subjectData} which defines the dose
#' @param tox the name of the column in \code{subjectData} which defines the toxicity status.
#' @param doseGrid the values to appear along the dose axis.  Can be omitted: if so, only those
#' values that exist in \code{dose} column of \code{subjectData} will be used.
#' @param toxCategories a list containing the labels of the toxicity categories that will appear
#'  in the legend.  Can be omitted.  If omitted, sensible defaults are derived, based on the
#'  mighest value of \code{tox} in \code{subjectData}.  Names of list elements should be "0" ... "<n>",
#'  values are used as labels in the legend.
#' @param toxData the name of the tibble containing the poasterior estimates of toxicity.
#'   See Usage notes below.
#' @param toxVar the name of the column in \code{toxData} containing p(Tox) [or other relevant statistic].
#'   See Usage notes below
#' @param recommendedDose Optional.  The value of \code{dose} to mark as the recommended dose
#' @section Usage notes:
#' \code{toxData} and \code{toxVar} must either both be omitted or both be present.
#'    The \code{toxData} tibble should contain columns named \code{dose} and \code{toxVar}.
#'    Other columns are ignored.  It should contain one row for each value of \code{dose} in
#'    \code{subjectData}
#' @examples
#' #Basic usage
#' data("oQuigleyPatientData")
#' allocationPlot(oQuigleyPatientData, doseGrid=1:6)
#' #More advanced
#' data("oQuigleyPosteriorShort")
#' toxData <- oQuigleyPosteriorShort %>% tidy() %>% augmentOQuigleyData() %>% createDoseSummary()
#' allocationPlot(oQuigleyPatientData, doseGrid=1:6, toxData=toxData, toxVar=Mean, recommendedDose=2)
#' @importFrom rlang enquo
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_fill_hue
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 sec_axis
#' @importFrom dplyr mutate
#' @importFrom tibble is_tibble
#'
#' @export
allocationPlot <- function(
                    subjectData,
                    dose=Dose,
                    tox=Toxicity,
                    doseGrid,
                    toxCategories=list(),
                    toxData=NA,
                    toxVar=NA,
                    recommendedDose=NA
                  ) {

  qDose <- enquo(dose)
  qTox <- enquo(tox)
  d <- subjectData
  # If necessary convert tox to a factor and deduce sensible category labels and scale colours
  if (!is.factor(d[rlang::as_name(qTox)])) {
    if (length(toxCategories) == 0) {
      n <- max(d[rlang::as_name(qTox)]) + 1
      if (n < 3) {
        toxCategories <- list("1"="Tox", "0"="None")
        toxScale <- scale_fill_manual(values=c("firebrick", "grey"))
      }
      else if (n == 3) {
        toxCategories <- list("2"="Tox", "1"="SubTox", "0"="None")
        toxScale <- scale_fill_manual(values=c("firebrick", "orange1", "grey"))
      } else{
        for (i in seq(n-1, 1, -1)) {
          toxCategories[[as.character(i)]] <- paste0("Cat", i)
        }
        toxCategories[["0"]] <- "None"
        toxScale <- scale_fill_hue()
      }
    }
    d <- d %>% mutate(!! qTox := factor(!! qTox, levels=names(toxCategories), labels=toxCategories))
  }
  p <- d %>% ggplot() +
    ggplot2::geom_bar(aes(x=!! qDose, fill=!! qTox)) +
    ggplot2::coord_cartesian(xlim=c(min(doseGrid), max(doseGrid))) +
    ggplot2::labs(y="Subject count") +
    toxScale +
    ggplot2::theme_light()
  if (is_tibble(toxData)) {
    qToxVar <- rlang::enquo(toxVar)
    # Work out transformation of toxicity scale to number of subjects scale.
    yRange <- ggplot2::ggplot_build(p)$layout$get_scales(1)$y$range$range
    p <- p +
      geom_line(toxData, mapping=aes(x=!! qDose, y=yRange[1] + (yRange[2] - yRange[1])*(!! qToxVar), colour="blue")) +
      scale_y_continuous(sec.axis=sec_axis(trans=~yRange[1] + . / (yRange[2] - yRange[1]), name='p(Tox)')) +
      scale_colour_manual(values="blue", labels=" ", name="Est p(Tox)")
  }
  if (!is.na(recommendedDose)) {
    p <- p +
      geom_point(aes(x=recommendedDose, y=0, shape=as.factor(17)), colour="limegreen", size=5) +
      scale_shape_manual(values=17, labels=" ", name="Rec dose")
  }
  return(p)
}

#' Summarise patient exposure by dose
#'
#' @param data the mcmc tibble to be summarised
#' @param dose the name of the column in \code{data} which defines the dose
#' @param tox the name of the column in \code{data} which defines the toxicity status.  See Usage notes below.
#' @param toxLabels the names of the variables tahat will contain the incidence counts in the output dataset.
#' See Usage notes below.
#' @param doseGrid the values of \code{dose} for which incidence counts will be included in the output dataset.
#' See Usage notes below.
#' @return a tibble containing one row for each value in \code{doseGrid} and columns named: the value of \code{dose},
#' \code{Treated}, \code{Evaluable}, and one for each element in \code{doseGrid}.  See Usage notes
#' below.
#' @section Usage notes:
#' \code{tox} should take integer values from 0 to N, with 0 indicating no toxicity and values of 1 or more indicating
#' increasing values of toxicity.
#' If \code{toxLabels} is \code{NA}, default labelds are created dependent on the number of distinct values of
#' \code{tox} found in \code{data}.  If there are one or two, labels are "None" and "DLT".  If there are exactly three,
#' labels are "None", "SubDLT" and "DLT".  If there are four or more, labels are "None, "Cat1", ..., "CatN".
#' \code{doseGrid} can be used to ensure that all relevant doses appear in the output dataset, even those
#'  with observed zero counts.
#' In the output dataset, \code{Treated} counts the number of observations with non-missing values of \code{dose};
#'  \code{Evaluable} counts the number of observations with non-missing \code{tox}; \code{None} counts the
#'  number of observations wuth \code{tox} equal to 1, and the other count variables count the number of observations
#'   with eachand one for each of the remaining values of \code{tox}.
#' @examples
#' data("oQuigleyPatientData")
#' patientSummary(oQuigleyPatientData, doseGrid=1:6)
#' @importFrom rlang :=
#' @export
patientSummary <- function(
  data,
  dose=Dose,
  tox=Toxicity,
  toxLabels=NA,
  doseGrid=NULL) {
  d_ <- rlang::enquo(dose)
  t_ <- rlang::enquo(tox)

  # Validate
  if (is.na(toxLabels)) {
    n <- data %>% dplyr::select(!! t_) %>% dplyr::distinct() %>% nrow()
    if (n < 3) {
      levels <- 0:1
      labels <- c("None", "Tox")
    } else if (n == 3) {
      levels <- 0:2
      labels <- c("None", "SubTox", "Tox")
    } else {
      levels <- 0:(n-1)
      labels <- c("None", paste0("Cat", 1:(n-1)))
    }
  }
  d <- data %>%
         dplyr::mutate(
           Label=factor(Toxicity, levels=levels, labels=labels),
           Treated=!is.na(!! d_),
           Evaluable=!is.na(!! t_)
         )
  if (is.null(doseGrid)) {
    doseGrid <- 0:((data %>% dplyr::select(!! d_) %>% dplyr::summarise(Max=max(!! d_)))$Max)
  }
  allDoses <- tibble::tibble(!!d_ := doseGrid)
  ptCounts <- d %>%
    dplyr::group_by(!! d_) %>%
    dplyr::summarise(
      Treated=sum(Treated),
      Evaluable=sum(Evaluable),
      .groups="drop"
    )
  toxCounts <- d %>%
    dplyr::group_by(!! d_, Label) %>%
    dplyr::summarise(N=dplyr::n(), .groups="drop") %>%
    tidyr::pivot_wider(
      names_from=Label,
      values_from=("N")
    )
  allDoses %>%
    dplyr::left_join(ptCounts, by=rlang::as_name(d_)) %>%
    dplyr::left_join(toxCounts, by=rlang::as_name(d_)) %>%
    replace(is.na(.), 0)
}

#' Create Summary Statistics from a MCMC Samples
#'
#' Given a tibble equivalent of a \code{mcmc} or \code{mcmc.list} object,
#' creates a tibble of summary statistics by dose.
#'
#' @param data the mcmc tibble to be summarised
#' @param group the name of the column in \code{data} which defines the grouping
#' @param ... the statistics to be produced.  See Usage notes below.
#' @param widen Should \code{df-col}s and \code{list} columns inthe return
#'  value be expanded?  See Usage notes below.
#' @return a tibble containing the requested summary statistics.  See Usage
#'  notes below.
#' @section Usage notes:
#' Statistics can be requested simply giving the appropriate R
#' function.  For example \code{Mean=mean(Value)}.  It is sometimes useful to
#' create a wrapper to improve the layout of the returned tibble.
#' \code{purrr::partial} can be helpful for this.  For example, to ensure that
#' quantiles are returned with integer suffices (eg Q1, Q2, rather than
#' Q`5%`,Q`10%` etc`) define \code{qFun <- partial(quantile, x=, q=c(.25, .5,
#'  .75), names=FALSE)} and then \code{createCustomDoseSummary(df, q=qFun(Value))}.
#' If \code{widen} is \code{TRUE}, `df-col`s and `list` columns are expanded.
#' Column names are given by the concatenation of the outer and inner column names.
#'   For example, in the quantile example above, column names qill be \code{Q1} to \code{Q3}.
#' @examples
#' #Equivalent to a call to createDoseSummary:
#' data(oCRMPosteriorShort)
#' createCustomDoseSummary(
#'  oCRMPosteriorShort %>% tidy() %>% augmentOCRMData(),
#'  N=sum(!is.na(Value)),
#'  Q=stats::quantile(Value, q=quantiles, names=FALSE),
#'  Mean=mean(Value, na.rm=TRUE)
#')
#' @export
createCustomDoseSummary <- function(data, ..., widen=TRUE, group=Dose) {
  .g <- rlang::enquo(group)
  fns <- rlang::enquos(...)
  fns <- purrr::map(fns, function(x) {
    res <- rlang::eval_tidy(x, data = data)
    if ( ((is.vector(res)  || is.factor(res)) && length(res) == 1) ||
         ("list" %in% class(res) && is.list(res)) ||
         rlang::call_name(rlang::quo_get_expr(x)) == "list") {
      x
    }
    else if ((is.vector(res)  || is.factor(res)) && length(res) > 1) {
      x_expr <- as.character(list(rlang::quo_get_expr(x)))
      x_expr <- paste0(
        "tidyr::pivot_wider(tibble::enframe(",
        x_expr,
        "), names_from = name, values_from = value)"
      )
      x <- rlang::quo_set_expr(x, str2lang(x_expr))
      x
    } else {
      x_expr <- as.character(list(rlang::quo_get_expr(x)))
      x_expr <- paste0("list(", x_expr,")")
      x <- rlang::quo_set_expr(x, str2lang(x_expr))
      x
    }
  })
  rv <- data %>%
    dplyr::group_by(!! .g) %>%
    dplyr::summarise(!!! fns, .groups="drop")
  if (widen) rv <- rv %>% widen()
  return(rv)
}

#'
widen <- function(df) {
  df_cols <- names(df)[purrr::map_lgl(df, is.data.frame)]
  df <- tidyr::unpack(df, tidyselect::all_of(df_cols), names_sep = "")
  try_tidy <- function(x) {
    tryCatch({
      broom::tidy(x)
    }, error = function(e) {
      x
    })
  }
  df <- df %>%
          dplyr::rowwise() %>%
          # .where() defined in utils.R.  Local copy of tidyverse:::where() - references to
          # unexported functions are not allowed on CRAN
          dplyr::mutate(dplyr::across(.where(is.list), try_tidy))
  dplyr::ungroup(df)
}

#' Create Summary Statistics from a MCMC Samples
#'
#' Given a tibble equivalent of a \code{mcmc} or \code{mcmc.list} object,
#' creates a tibble of standard summary statistics by dose.
#'
#' @param data the mcmc tibble to be summarised
#' @param group the name of the column in \code{data} which defines the grouping
#' @param quantiles the quantiles to be produced.
#' @param widen Should \code{df-col}s and \code{list} columns in the return
#'  value be expanded?  See Usage notes below.
#' @return a tibble containing the requested summary statistics.  See Usage
#'  notes below.
#' @section Usage notes:
#' \code{createDoseSummary} is a convenience wrapper around
#' \code{\link{createCustomDoseSummary}} that produces a standard summary tibble
#' that reports the mean and quantiles (defined by the \code{quantiles} parameter
#' by \code{group}.  The default action is to report the mean, 5th, 10th, 50th,
#' 90th and 95th centiles of \code{Value} grouped by \code{Dose}.  See
#'  \code{\link{createCustomDoseSummary}} for more details.
#' @examples
#' data(oCRMPosteriorShort)
#' x <- oCRMPosteriorShort %>% tidy() %>% augmentOCRMData()
#' createDoseSummary(x)
#' @export
createDoseSummary <- function(data, group=Dose, quantiles=c(0.05, 0.1, 0.5, 0.9, 0.95), widen=TRUE) {
  createCustomDoseSummary(
    data,
    N=sum(!is.na(Value)),
    Q=stats::quantile(Value, q=quantiles, names=FALSE),
    Mean=mean(Value, na.rm=TRUE),
    widen=widen
  )
}


#' Convert MCMC Sample Chains produced by \code{runjags} to a tibble
#'
#' If the class of \code{obj} is \code{runjags}, the \code{mcmc} field is
#' extracted and processed.  If \code{runjags.list} or \code{mcmc.list},
#' \code{tidy} calls itself recursively on each element of
#' \code{obj}, \code{row_bind()}s the results together and returns the
#' resulting tibble.  If \code{mcmc}, \code{obj} is processed as described in
#' Usage notes below.
#'
#' @param obj the object to be converted.  See Usage notes below.
#' @param chain the chain id
#' @param augment If not \code{NULL}, the function which augments the
#' converted \code{obj}.  The functions first argument should be the tidied version of
#' \code{obj}.  Other arguments can be defined in \code{...}.
#' with columns that are relevant to future processing.  See Usage notes below.
#' @param indexedPattern the regular expression used to identify indexed model
#' parameters.  Ensure that the pattern includes groups to return the "array"
#' name and the index.  The default value isn't perfect - it allows, for
#' example, _3a to be a valid variable name - but it sould be good enough for
#' most purposes.
#' @param ... Arguments passwed to \code{augment}.
#' @return the corresponding tibble.  See Usage notes below.
#' @section Usage notes:
#' Typically, the CRM model parameters are of little intrest in their own right.
#' The probabiities derived from them are more relevant.  If the JAGS code
#' derives these probabilities, they will be returned in the tibble as described
#' below.  If not, the \code{augment} argument provides a hook to allow them
#' to be derived.  \code{augment} is called immediately before the transformed
#' obj is returned, so it must process \code{obj} in its transformed state.
#' The transformed object is in normal form, which means that usually it must be
#' \code{spread()} or \code{pivot}ed \code{_wider} so that all model parameters are
#' available in a single row. This is slightly awkward, but takes little time.
#' See the vignette for an example.
#'
#' Assuming that \code{augment} is \code{NULL}, the tibble returned by this
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
#' \code{Thin}: The value of the thinning parameter of the MCMC process used to generate
#' the samples
#' of this \code{Chain}.
#'
#' If \code{augment} is not \code{NULL}, the return value depends on the
#' actions of \code{augment}.
#' @examples
#' data(nCRMPosteriorShort)
#' tidy(nCRMPosteriorShort)
#' @importFrom rlang .data
#' @export
tidy <- function(
          obj,
          chain=1,
          augment=NULL,
          indexedPattern="([:alnum:\\.]*)\\[(\\d*)\\]",
          ...
        ) {
  if (is.null(obj)) {
    warning("Object is null or NA.  Nothing to do.  Returning NA...")
    return(NA)
  }
  if ("runjags" %in% class(obj)) {
    return(tidy(obj$mcmc, chain, augment, indexedPattern))
  }
  if ("runjags.list" %in% class(obj)) {
    return (
      dplyr::bind_rows(
        lapply(
          1:length(obj),
          function(i) tidy(obj[[i]], i,augment, indexedPattern)
        )
      )
    )
  }
  if ("mcmc.list" %in% class(obj)) {
    return (
      dplyr::bind_rows(
        lapply(
          1:length(obj),
          function(i) tidy(obj[[i]], i, augment, indexedPattern)
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
  rv <- dplyr::bind_rows(
          lapply(
            1:length(colNames),
            function(i) {
              cbind(base) %>%
                tibble(
                  Temp=colNames[i],
                  Value=as.double(obj[,colNames[i]],
                  Thin=a$mcpar[3]
                )
              )
            }
          )
        )
  rv <- rv %>%
          tidyr::separate(
            col=Temp,
            into=c("Parameter", "Index", NA),
            sep="([\\[\\]])",
            convert=TRUE,
            fill="right") %>%
          dplyr::arrange(Chain, Sample, Parameter, Index)
  if (!is.null(augment)) {
    if (!is.function(augment)) stop(paste0("'augment' must be a function, not '", typeof(augment), "."))
    rv <- augment(rv, ...)
  }
  return (rv)
}
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
#' data(oCRMPatientData)
#' subjectGraph(oCRMPatientData, type="ocrm")
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
#' @param data the input tibble, created by \code{createDoseSummary}
#' @param pivot Should \code{data} be normalised?  If \code{NA}, the default,
#' \code{data} is normalised if it does not contain only columns \code{dose},
#' \code{Statistic} and \code{Value}.  A warning is printed if pivoting is required.
#' If \code{TRUE}, \code{data} is pivoted only if necessary, but no warning is printed.
#' If \code{FALSE}, \code{data} is never normalised and an error occurs if the required
#' columns are not found.
#' @param dose the column in \code{data} that defines the dose
#' @param lines a vector of \code{Parameter}s in \code{data} whose \code{Value}s
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
#' data(oCRMPosteriorShort)
#' x <- oCRMPosteriorShort %>%
#'        tidy() %>%
#'        augmentOCRMData() %>%
#'        dplyr::filter(Parameter == "Prob", Index == 2)
#' doseToxicityGraph(
#'   createDoseSummary(x) %>% dplyr::select(-N),
#'   targetTox=c(0.15, 0.30),
#'   pivot=TRUE)
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
doseToxicityGraph <- function(
                       data,
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
                       defaultLegends=TRUE,
                       pivot=NA) {
  d_ <- dplyr::enquo(dose)
  #Validate
  d <- data
  if (!methods::is(data, "tbl")) stop("data must be a tibble.")
  if (!isTRUE(all.equal(colnames(data), c("Dose", "Statistic", "Value")))) {
    if (is.na(pivot)) {
      warning("Pivoting to normalise input data.  To suppress this warning, supply a normalised dataset or set pivot=TRUE")
      pivot <- TRUE
    }
    if (!pivot) stop("data must be normalised or pivot must be TRUE")
    d <- data %>% tidyr::pivot_longer(values_to="Value", names_to="Statistic", cols=c(-(!! d_)))
  }
  # if (length(unique(data$Parameter)) > 1) warning("More than one Parameter in data.  Results are likely to be misleading.")

  #Rename the variable containing the 50th centile to (by default) Median, if necessary
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
        tibble::add_column(
          Group=ribbonNames[i],
          Ribbon=alphaList[j],
          Colour=colours[Group]
        )
    }
    ribbonData[[i]] <- dplyr::bind_rows(rData)
  }
  ribbonData <- dplyr::bind_rows(ribbonData)

  ribbonRequests <- ggplot2::geom_ribbon(
                      data=ribbonData,
                      ggplot2::aes(
                        x=Dose,
                        ymin=Min,
                        ymax=Max,
                        group=Ribbon,
                        fill=Group,
                        alpha=Ribbon
                      ),
                      fill=ribbonData$Colour
                    )

  #Build the plot.  (Do it this way to control layer order.)
  plot <- ggplot2::ggplot()
  if (!is.null(toxRequest)) plot <- plot + toxRequest
  if (!is.null(ribbonRequests)) plot <- plot + ribbonRequests
  if (!is.null(lineRequests)) plot <- plot + lineRequests

  plot <- plot +
            ggplot2::theme_light() +
            ggplot2::labs(
              x=xLabel,
              y=yLabel
            )

  if (defaultLegends) {
    lineColours <- colours[tolower(lines)]
    names(lineColours) <- NULL
    plot <- plot +
      ggplot2::scale_colour_manual(
        name=ifelse(is.null(labels["lines"]), " ", labels["lines"]),
        labels=lines,
        values=lineColours
      )
    if (!is.null(ribbons)) {
      for(i in length(ribbons)) {
        lbl <- ifelse(is.null(labels[ribbonNames[i]]), ribbonNames[i], labels[ribbonNames[i]])
        catLabels <- unname(labels[grepl(paste0(ribbonNames[i], "\\d+"), names(labels))])
        #names(catLabels) <- NULL
        breakList <- seq(from=alphaRange[1], to=alphaRange[2], length.out=2)
        suppressWarnings(
          plot <- plot +
                    ggplot2::scale_alpha(
                      name=labels[ribbonNames[i]],
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
