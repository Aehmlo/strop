#' Bootstrap quantiles
#'
#' @param x A bootstrap object
#' @param probs Vector of probabilities
#' @param ... Additional arguments passed to \code{quantile}
#'
#' @importFrom stats quantile
#'
#' @return A vector of quantiles
#' @export
#'
#' @examples
#' data(mtcars)
#' obj <- bootstrap(mtcars$cyl)
#' quantile(obj, 0.5)
quantile.strop <- function(x, probs, ...) {
    if (is.nested(x$stats)) {
        lapply(x$stats, function(x) quantile(x, probs, ...))
    } else {
        quantile(x$stats, probs, ...)
    }
}
