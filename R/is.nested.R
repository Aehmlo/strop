is.nested <- function(l) {
    is.list(l) & all(unlist(lapply(l, function(x) length(x) > 1)))
}
