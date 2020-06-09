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
      p <- p + geom_vline(aes(xintercept=targetTox), colourl="green", show.legend=FALSE)
    }
  }
  return(p)
}
