#' return knot
#'
#' @param fit fit
#'
#' @return knot
#' @export
#'
getKnot <- function(fit){
    not <- c(paste0(rcsx(fit),''),
      paste0(rcsx(fit),"'"),
      paste0(rcsx(fit),do::rep_n("'",2)),
      paste0(rcsx(fit),do::rep_n("'",3)),
      paste0(rcsx(fit),do::rep_n("'",4)),
      paste0(rcsx(fit),do::rep_n("'",5)),
      paste0(rcsx(fit),do::rep_n("'",6)),
      paste0(rcsx(fit),do::rep_n("'",7)),
      paste0(rcsx(fit),do::rep_n("'",8)),
      paste0(rcsx(fit),do::rep_n("'",9)),
      paste0(rcsx(fit),do::rep_n("'",10)))
    which.min(not %in% names(fit$coefficients))
}
