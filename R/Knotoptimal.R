#' optimal Knot for rcs() function
#'
#' @param fit result of fit from 'rms' package
#' @param n number of nots
#' @param plot logical. whether to plot
#'
#' @return AIC and plot
#' @export
#'
optimalKnot <- function(fit,n=3:10,plot=TRUE,title=NULL,data=NULL){

    (rcsx <- rcsx(fit))
    (formu <- paste0(deparse(fit$call$formula),collapse = ''))
    (comb <- combn(n,length(rcsx)) |>data.frame())
    fs <- lapply(n,function(i){
        uf <- updateKnot(fit,i,data=data)
        cbind(data.frame(matrix(i,nrow=1)) |> do::give_names(paste0(rcsx,'_knot')),
              data.frame(AIC=AIC(uf),P_nonlinear = anova(uf)[' Nonlinear','P']))
    }) |> do.call(what = rbind)
    fs$min <- ''

    if (any(fs$P_nonlinear <=0.05)){
        ck <- fs$P_nonlinear <=0.05
        p <- fs$P_nonlinear
        p[!ck] <- 10
        fs$min[which.min(p)] <- '***'
    }else{
        fs$min[which.min(fs$AIC)] <- '***'
    }

    row.names(fs) <- NULL
    plot(fs[,1],y=fs$AIC,type = 'n',xlab = paste0(rcsx,'-->knot'),ylab = 'AIC')
    title(title)
    colors <- rep('blue',length(fs[,1]))
    colors[fs$P_nonlinear <= 0.05] <- 'red'
    points(fs[,1],fs$AIC,pch=16,col=colors)
    # axis(side = 1, at=N)
    fs
}


