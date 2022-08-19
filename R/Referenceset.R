#' set adjustment value
#'
#' @param x variable
#' @param value value
#'
#' @return data distribution
#' @export
#'
setReference <- function(x,value){
    dn <- options()$datadist
    if (is.data.frame(x)){
        rcsx <-attr(x,'rcsx')
        for (i in rcsx) {
            value <- x[x$rcsName == i,i]
            value <- ifelse(is.numeric(value),value ,sprintf("'%s'",value))
            eval(parse(text=sprintf("%s[['limits']]['Adjust to','%s'] <- %s",dn,i,value)),envir = .GlobalEnv)
            eval(parse(text = dn),envir = .GlobalEnv)
        }
    }else{
        value <- ifelse(is.numeric(value),value ,sprintf("'%s'",value))
        eval(parse(text=sprintf("%s[['limits']]['Adjust to','%s'] <- %s",dn,x,value)),envir = .GlobalEnv)
        eval(parse(text = dn),envir = .GlobalEnv)
    }

}
