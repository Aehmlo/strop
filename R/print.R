#' Bootstrap result printing
#'
#' @param x A boostrap object of class \code{strop}
#' @param max The maximum number of bootstrap estimates to print
#' @param ... Additional arguments
#'
#' @export
#'
#' @examples
#' data(mtcars)
#' print(bootstrap(mtcars$cyl))
print.strop <- function(x, max = 10, ...) {
    obj <- x
    cl <- as.character(obj$call)
    fn <- cl[1]
    args <- cl[-1]
    cat(paste("Call:", paste0(fn, "(", paste(args, collapse = ", "), ")"), "\n", sep = "\n"))
    if (length(obj$pop) > 1) {
        ests <- sapply(obj$stats, function(x) head(x, max))
    } else {
        ests <- head(obj$stats, max)
    }
    cat("Sample estimates:\n")
    print(obj$pop)
    cat("\nBootstrap estimates:\n")
    print(ests)
    if(length(obj$stats[[1]]) > max) {
        cat(paste0("(", length(obj$stats[[1]]) - max, " omitted)"))
    }
}
