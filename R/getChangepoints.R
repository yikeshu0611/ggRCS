#' getChangepoints
#'
#' @param r result of RCS()
#' @param range range for the rcs() varaibel
#'
#' @return dataframe
#' @export
#'
getChangepoints <- function(r,range=NULL) UseMethod('getChangepoints')

#' @export
#' @method getChangepoints m1
#'
getChangepoints.m1 <- function(r,range=NULL){
    rcsx <- attr(r,'rcsx')
    if (!is.null(range)) r <- r[r[,rcsx] >= min(range) & r[,rcsx] <= max(range),]
    sapply(2:(nrow(r)-1),function(i){
        y0 <- r$yhat[i]-r$yhat[i-1]
        x0 <- r[,rcsx][i]-r[,rcsx][i-1]
        delt1 <- y0/x0

        y2 <- r$yhat[i+1]-r$yhat[i]
        x2 <- r[,rcsx][i+1]-r[,rcsx][i]
        delt2 <- y2/x2

        if(delt1 * delt2 <0) return(r[i,c(rcsx,'yhat')])
        if (delt2 == 0) return(r[i+1,c(rcsx,'yhat')])
    }) |> do.call(what = rbind) |> unique()
}
#' @export
#' @method getChangepoints m1by
#'
getChangepoints.m1by <- function(r,range=NULL){
    (rcsx <- attr(r,'rcsx'))
    (by <- attr(r,'by'))
    if (!is.null(range)) r <- r[r[,rcsx] >= min(range) & r[,rcsx] <= max(range),]
    lapply(unique(r[,by]),function(bi){
        ri <- r[r[,by]  %in% bi  ,]
        lapply(2:(nrow(ri)-1),function(i){
            y0 <- ri$yhat[i]-ri$yhat[i-1]
            x0 <- ri[,rcsx][i]-ri[,rcsx][i-1]
            (delt1 <- y0/x0)

            y2 <- ri$yhat[i+1]-ri$yhat[i]
            x2 <- ri[,rcsx][i+1]-ri[,rcsx][i]
            (delt2 <- y2/x2)

            if(delt1 * delt2 * 10000000 <0){
                res <- data.frame(by=bi,rcsx=ri[,rcsx][i],yhat=ri$yhat[i])
                colnames(res)[1:2] <- c(by,rcsx)
                res
            }else if (delt2 == 0) {
                res <- data.frame(by=bi,rcsx=ri[,rcsx][i+1],yhat=ri$yhat[i])
                colnames(res)[1:2] <- c(by,rcsx)
                res
            }
        }) |> do.call(what = rbind)
    }) |> do.call(what = rbind)

}
