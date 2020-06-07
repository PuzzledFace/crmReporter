#' Create a Plot of the Posterior PDFs by Dose
#' @param d the tibble containing the posterior MCMC samples, augmented by dose,
#' in normal form
#' @param dose the column in \code{d} that defines dose
#' @param value the column in \code{d} that defines the probabiity of toxicity
#' @param xLabel the text to label the x axis
#' @param yLabel the text to label the y axis
#' @param dTitle the title of the legend describing the doses
#' @param dLabels the texts used to label the doses.  If \code{NA}, the raw
#' values of the \code{dose} column are used
#' @param xLimits a two element vector defining the limits of the x axis.  See
#' Usage nots below.
#' @param defaultFormatting apply default formatting to the axes and legend?
#' See Usage notes below.
#' @param ... other parameters passed to \code{geom_density()}
#' @return a ggplot2 object which may be further customised
#' @section Usage notes:
#' The range of the x axis is limited by \code{coord_cartesian()}, so all rows
#' of \code{d} are used to compute the densities
#'
#' If \code{default Formatting == FALSE}, the values of \code{xLabel},
#' \code{yLabel}, \code{dTitle} and \code{dLabels} are ignored.
#' @examples
#' z <- runjagsToTibble(generateOCRMData(chainLength=1000)[["posterior"]])
#' z <- augmentOCRMData(z, doseGrid=4:10)
#' densityGraph(z, dose=Dose, value=Prob2)
#' @export
densityGraph <- function(d,
                         dose=Dose,
                         value=.data$Value,
                         xLabel="p(Toxicity)",
                         yLabel=" ",
                         dTitle="Dose",
                         dLabels=NULL,
                         xLimits=c(0, 1),
                         defaultFormatting=TRUE,
                         ...) {
  #Set up
  d_ <- dplyr::enquo(dose)
  v_ <- dplyr::enquo(value)
  #Initialise
  dBreaks <- (d %>% dplyr::distinct(!! d_))[[1]]
  if (is.null(dLabels)) dLabels <- dBreaks
  #Begin
  plot <-   d %>%
              ggplot2::ggplot() +
                ggplot2::geom_density(ggplot2::aes(x=!! v_,
                                                   colour=as.factor(!! d_)),
                                      ...) +
                ggplot2::coord_cartesian(xlim=xLimits) +
                ggplot2::theme_light()
  if (defaultFormatting) {
      if (is.function(dLabels)) dLabels <- dLabels(d)
      plot <- plot +
                ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                               axis.ticks.y=ggplot2::element_blank()) +
                ggplot2::labs(x=xLabel,
                              y=yLabel) +
                ggplot2::scale_colour_discrete(name=dTitle,
                                               breaks=dBreaks,
                                               labels=dLabels)
  }
  return (plot)
}
