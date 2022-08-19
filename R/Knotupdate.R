#' update Knot
#'
#' @param fit fit
#' @param k one number
#'
#' @return updated regression with new knot
#' @export
#'
updateKnot <- function(fit,k,data=NULL){
    rcsx <- rcsx(fit)
    f0 <- paste0(deparse(fit$call$formula),collapse = '')
    f1 <- sub(paste0('rcs\\(',rcsx,'[a-zA-Z0-9, \\._]{0,}\\)'),
              paste0('rcs\\(',rcsx,',',k,')'),
              f0) |> as.formula()
    if (is.null(getOption("datadist"))){
        update(fit,formula. = f1)
    }else if(is.null(.GlobalEnv[[getOption("datadist")]])){
        .GlobalEnv[[getOption("datadist")]] <- rms::datadist(do::model.data(fit))
        update(fit,formula. = f1)
        .GlobalEnv[[getOption("datadist")]] <- NULL
    }else{
        if (is.null(data)){
            update(fit,formula. = f1)
        }else{
            update(fit,formula. = f1,data=data)
        }
    }
}
