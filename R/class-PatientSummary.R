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
  if (length(unique(data[quo_name(t_)])) > length(toxLabels)) stop("More toxicity categories than labels")
  if (max(data[quo_name(t_)]) >= length(toxLabels)) stop("Inconsistent toxicity category values")
  #if (length(unique(data[quo_name(t_)])) != length(toxLabels)) warning("More toxicity labels than categories")

  #Begin
  d <- data %>%
         group_by(!! d_) %>%
         summarise(Enrolled=n(),
                   Evaluable=sum(!is.na(!! s_)))

  d1 <- data %>%
          mutate(Temp_=factor(!! t_, levels=0:(length(toxLabels)-1), labels=str_replace_all(toxLabels, fixed(" "), ""))) %>%
          group_by(!! d_, Temp_) %>%
          summarise(N=n()) %>%
          spread(key=Temp_, value=N, fill=0)

  d <- d %>% full_join(d1, by=quo_name(d_))
  return(.PatientSummary(summary=d,
                         columnLabels=c("Dose", "Enrolled", "Evaluable", toxLabels),
                         footnotes=footnotes))
}

setMethod("initialize",
          "PatientSummary",
          function (.Object, ...) {
            args <- list(...)
            .Object <- callNextMethod()
            .Object@summary <- args[["summary"]]
            .Object@columnLabels <- args[["columnLabels"]]
            .Object@footnotes <- args[["footnotes"]]
            return(.Object)
          })
