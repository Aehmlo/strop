#' Bootstrap visualization
#'
#' @param x A boostrap object of class \code{strop}
#' @param conf.level The confidence level for which to indicate central confidence intervals on each histogram
#' @param combine Whether to attempt to combine plots
#' @param vars The variables for which to produce plots
#' @param ... Additional arguments
#'
#' @importFrom graphics abline hist layout
#' @importFrom stats coef update
#' @importFrom utils head
#'
#' @return \code{invisible(x)}
#' @export
#'
#' @examples
#' data(mtcars)
#' plot(bootstrap(mtcars$cyl))
plot.strop <- function(x, conf.level = 0.95, combine = F, vars = NA, ...) {
    obj <- x
    fun <- ifelse(is.null(obj$call$FUN), "means", obj$call$FUN)
    if (is.list(obj$stats)) {
        stats <- names(obj$stats)
        n <- length(stats)
        width <- floor(sqrt(n))
        height <- ceiling(sqrt(n))
        if(combine) {
            layout(matrix(1:n, nrow = height, ncol = width, byrow = T))
        }
        for (i in 1:n) {
            termname <- stats[i]
            stat <- unlist(obj$stats[[termname]])
            if (anyNA(vars) | is.element(termname, vars)) {
                hist(stat, main = termname, xlab = "Sample statistics")
                abline(v = quantile(stat, (1 + c(-1, 1) * conf.level) / 2), lty = "dashed")
            }
        }
        if(combine) {
            layout(matrix(1))
        }
    } else {
        hist(obj$stats, main = paste("Histogram of", fun), xlab = "Sample statistics")
        abline(v = confint(obj, level = conf.level), lty = "dashed")
    }
    invisible(x)
}
