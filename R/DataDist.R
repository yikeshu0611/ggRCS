#' modify datadist
#'
#' @param ... see datadist[rms]
#' @param data see datadist[rms]
#' @param q.display see datadist[rms]
#' @param q.effect see datadist[rms]
#' @param adjto.cat see datadist[rms]
#' @param n.unique see datadist[rms]
#'
#' @return  see datadist[rms]
#' @export
#'
DataDist <- function(..., data, q.display, q.effect = c(0.25, 0.75),
                     adjto.cat = c("mode",
                                   "first"),
                     n.unique = 10){
    adjto.cat <- match.arg(adjto.cat)
    dd <- rms::datadist(..., data, q.display, q.effect, adjto.cat , n.unique = n.unique)
    .GlobalEnv[[getOption("datadist")]] <- dd
}
