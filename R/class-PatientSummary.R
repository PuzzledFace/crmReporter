.PatientSummary <- setClass("PatientSummary",
                         slots=c("summary"="tbl",
                                 "columnLabels"="vector",
                                 "footnotes"="list"),
                         validity=function(object){
                           return (TRUE)
                         })

PatientSummary <- function(data,
                        dose=Dose,
                        subject=SubjectID,
                        toxicity=Toxicity,
                        toxLabels=c("Without DLTs", "With DLTs"),
                        subjLabel="Subjects",
                        footnotes=list(),
                        ...) {
  #Initialise
  d_ <- dplyr::enquo(dose)
  s_ <- dplyr::enquo(subject)
  t_ <- dplyr::enquo(toxicity)

  #Check
  if (length(unique(data[dplyr::quo_name(t_)])) > length(toxLabels)) stop("More toxicity categories than labels")
  if (max(data[dplyr::quo_name(t_)]) >= length(toxLabels)) stop("Inconsistent toxicity category values")

  #Begin
  d <- data %>%
    dplyr::group_by(!! d_) %>%
    dplyr::summarise(
      Enrolled=dplyr::n(),
      Evaluable=sum(!is.na(!! s_))
    )

  d1 <- data %>%
          dplyr::mutate(
            Temp_=factor(
                    !! t_,
                    levels=0:(length(toxLabels)-1),
                    labels=stringr::str_replace_all(toxLabels,   stringr::fixed(" "), "")
                  )
          ) %>%
          dplyr::group_by(!! d_, Temp_) %>%
          dplyr::summarise(N=dplyr::n()) %>%
          tidyr::spread(key=Temp_, value=N, fill=0)

  d <- d %>% dplyr::full_join(d1, by=dplyr::quo_name(d_))
  return(
    .PatientSummary(
      summary=d,
      columnLabels=c("Dose", "Enrolled", "Evaluable", toxLabels),
      footnotes=footnotes)
  )
}

setMethod(
  "initialize",
  "PatientSummary",
  function (.Object, ...) {
    args <- list(...)
    .Object <- methods::callNextMethod()
    .Object@summary <- args[["summary"]]
    .Object@columnLabels <- args[["columnLabels"]]
    .Object@footnotes <- args[["footnotes"]]
    return(.Object)
  }
)
