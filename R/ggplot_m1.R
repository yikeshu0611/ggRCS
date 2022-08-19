#' @title Plot for decision curve
#' @param data resultes of dca() function
#' @param mapping ignore
#' @param color logical or colors
#' @param linetype logical or integers
#' @param lwd logical or integers
#' @param ... ignore
#' @param environment ignore
#' @rdname ggplot
#' @importFrom ggplot2 ggplot aes_string geom_line xlim ylim theme_classic xlab ylab element_blank theme
#' @importFrom ggplot2 geom_ribbon facet_wrap scale_size_manual scale_linetype_manual scale_color_manual
#' @importFrom rms cph
#' @method ggplot m1
#' @export
#'
ggplot.m1 <- function(data,
                      mapping,
                      ...,
                      environment = parent.frame(),
                      vline=TRUE,
                      hline=TRUE,
                      point=FALSE){
    x = unique(data$rcsName)
    p <- ggplot()+
        geom_line(data=data,
                  aes_string(x,'yhat'),
                  linetype="solid",
                  size=1,
                  alpha=0.7,
                  colour="red")+
        geom_ribbon(data=data,
                    aes_string(x,
                               ymin='lower',
                               ymax='upper'),
                    alpha=0.1,
                    fill="red")+
        theme_classic()
    if (isTRUE(vline)){
        p <- p + geom_vline(xintercept = unqiue(data$Ref),linetype=2)
        px <- unqiue(data$Ref)
    }else if(is.numeric(vline)){
        p <- p + geom_vline(xintercept = vline,linetype=2)
        px <- vline
    }
    if (isTRUE(hline)){
        p <- p + geom_hline(yintercept=1,size=0.75)
        py <- 1
    }else  if(is.numeric(hline)){
        p <- p + geom_hline(yintercept=hline,size=0.75)
        py <- hline
    }
    if (point)  p <- p + geom_point(aes(x=px,y=py),size=2)
    p

}











