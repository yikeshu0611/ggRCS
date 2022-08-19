
rcsx <- function(fit){
    names(fit$assign)[startsWith(names(fit$assign),'rcs')] |>
        do::Replace0(' {0,}\\,.*',' {0,}\\).*','rcs\\(') |> unique()
}
