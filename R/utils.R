#' local copy of tidyselect:::where, which is not excported, but needed.
#' @param fn the function used to filter the object
.where <- function (fn)
{
  predicate <- rlang::as_function(fn)
  function(x, ...) {
    out <- predicate(x, ...)
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    out
  }
}

modifyQuantileLabels <- function(labels,
                                 quantiles,
                                 pattern="Q(\\d+)",
                                 formatString="%4.1f%%",
                                 replacement="\\1",
                                 multiplier=100,
                                 median=TRUE) {
  #Validate
  if (is.null(labels)) stop("labels cannot be NULL")
  if (is.null(quantiles)) stop("quantiles cannot be NULL")
  if (!is.character(labels)) stop ("labels must be character")
  if (!is.numeric(quantiles)) stop("quantiles must be numeric")
  if (min(quantiles < 0)) warning("At least one negative quantile: are you sure?")
  if (max(quantiles > 1)) warning("At least one quantile > 1: are you sure?")
  #Begin
  stringr::str_replace(labels,
                       pattern,
                       function(x) {
                         i <- as.integer(stringr::str_replace(x, pattern, replacement))
                         newLabels <- stringr::str_trim(sprintf(formatString,
                                                                multiplier*quantiles[i]))
                         m <- which(quantiles == 0.5)
                         newLabels[which(i == m)] <- "Median"
                         return (newLabels)
                       })

}

# assignClass <- function(d) {
#   #Validate
#   if (!methods::is(d, "tbl")) stop("Unable to assign crmReporter class to an object that is not a tibble")
#
#   #Set up
#   validClasses <- c("crmSubjects", "oCRMSubjects", "ocrmPosterior", "crmPosterior")
#   #Begin
#   names <- names(d)
#   cls <- class(d)
#
#   if ("crmReporter" %in% cls && grepl(paste(validClasses, sep="|"), cls)) {
#     print("crmReporter classes already assigned.  Nothing to do...")
#     return(d)
#   }
#   if ("SubjectID" %in% names) {
#     #It's a subject data tibble
#     if ("Toxicity" %in% names) {
#       class(d) <- c("ocrmSubjects", "crmReporter", cls)
#       return(d)
#     } else {
#       class(d) <- c("crmSubjects", "crmReporter", cls)
#       return(d)
#     }
#   } else {
#     #It's a posterior tibble
#     if ("alpha1" %in% unique(d$Parameter)) {
#       class(d) <- c("ocrmPosterior", "crmReporter", cls)
#       return(d)
#     } else {
#       class(d) <- c("crmPosterior", "crmReporter", cls)
#       return(d)
#     }
#     warning(paste("Unhandled crmReporter type:", cls))
#     return(d)
#   }
#
# }
