#' adjsut reference for rcs() variable
#' @details
#' - for cph() and lrm(), value of x whith yhat close to 1
#' - for ols(), value of x whith yhat close to 0
#' @param fit result of cph(), lrm() or ols()
#'
#' @return dataframe
#' @export
#'
getReference <- function(fit,data=NULL){

    old <- options()

    options(datadist = '.zhishi.getReference')
    if (is.null(data)) data <- do::model.data(fit)
    .GlobalEnv[['.zhishi.getReference']] <-rms::datadist(data)

    rcsx <- rcsx(fit)
    x <- eval(parse(text=sprintf(fmt = 'Predict(fit,%s,fun=exp,ref.zero=TRUE)',rcsx)))

    rm(.zhishi.getReference,envir = .GlobalEnv)
    if (is.null(old$datadist)) options(datadist = NULL)
    options(old)

    x$rcsName <- rcsx
    x$method <- deparse(fit$call[[1]])
    class(x) <- 'data.frame'
    x

    px <- x$rcsName
    xir <- NULL
    for (i in unique(px)) {
        xi <- x[px == i,]
        if (any(xi$method  %in%  c('cph','lrm'))){
            minrow <- which.min(abs(xi$yhat-1))
            xir <- rbind(xir,xi[minrow,])
        }
        if (any(xi$method  %in%  c('ols'))){
            minrow <- which.min(abs(xi$yhat-0))
            xir <- rbind(xir,xi[minrow,])
        }
    }
    attr(xir,'rcsx') <- rcsx(fit)

    xir
}
