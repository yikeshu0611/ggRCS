start_with <- function(x,start){
    ck <- lapply(start, function(i) startsWith(x,i)) |> data.frame() |> rowSums()
    x[ck>0]
}
