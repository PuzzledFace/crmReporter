#' Create a Tibble Describing an Increments Rule
#'
#' @param object the \code{Increments} rule to be described
#' @return the tibble containing the description
#' @section Usage notes:
#' The tibble contains five columns:
#' Type: a description of the class of the rule
#' LowerLimit: the lower limit of the dose range to which the \code{MaxIncrement}
#' applies
#' UpperLimit: the upper limit of the dose range to which the \code{MaxIncrement}
#' applies
#' MaxIncrement: the maximum increment for this dose range, expressed as a
#' decimal fraction
#' MaxIncrementPct: the maximum increment for this dose range, expressed as a
#' percentage
#' Lower limits are inclusive, upper limits are exclusive
#' @examples
#' if (requireNamespace("crmPack")) {
#'   inc <- crmPack::IncrementsRelative(intervals=c(0, 20), increments=c(1, 0.33))
#'   summariseIncrementsRule(inc)
#' }
#' @importFrom methods .valueClassTest
#' @export
setGeneric("summariseIncrementsRule", valueClass = "tbl", function(object) {
  standardGeneric("summariseIncrementsRule")
})

#' Convert a tibble summarising an \code{Increments} rule
#'
#' @param object An \code{Increments} object
#' @rdname summariseIncrementsRule
#' @import crmPack
#' @export
setMethod("summariseIncrementsRule", signature("IncrementsRelativeDLT"), function(object) {
  rv <- tibble::tibble(
          Type="Relative DLT",
          LowerLimit=object@DLTintervals,
          UpperLimit=dplyr::lead(LowerLimit),
          MaxIncrement=object@increments,
          MaxIncrementPct=MaxIncrement*100
        )
  rv$UpperLimit[nrow(rv)] <- Inf
  return(rv)
})

#' @rdname summariseIncrementsRule
#' @export
setMethod("summariseIncrementsRule", signature("IncrementsRelativeParts"), function(object) {
  stop("Not implemented yet")
})

#' @rdname summariseIncrementsRule
#' @import crmPack
#' @export
setMethod("summariseIncrementsRule", signature("IncrementsRelative"), function(object) {
  rv <- tibble::tibble(
          Type="Relative",
          LowerLimit=object@intervals,
          UpperLimit=dplyr::lead(LowerLimit),
          MaxIncrement=object@increments,
          MaxIncrementPct=MaxIncrement*100)
  rv$UpperLimit[nrow(rv)] <- Inf
  return(rv)
})

