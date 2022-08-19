histgram_range <- function(x,bins){
    x <- x[!is.na(x)]
    x_range <- range(x)
    width <- (x_range[2] - x_range[1])/(bins - 1)
    boundary <- width/2
    shift <- floor((x_range[1] - boundary)/width)
    origin <- boundary + shift * width
    max_x <- x_range[2] + (1 - 1e-08) * width
    breaks <- seq(origin, max_x, width)
    cut(x = x, breaks = breaks, right = TRUE,
        include.lowest = TRUE) |> table() |> range()
}
