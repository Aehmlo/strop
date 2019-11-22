#' Symmetric bootstrap confidence intervals
#'
#' @param object A bootstrap object
#' @param parm Ignored
#' @param level The confidence level for the interval
#' @param ... Additional arguments passed to \code{quantile}
#'
#' @importFrom stats confint
#'
#' @return A symmetric \code{1 - level} confidence interval based on bootstrap resampling
#' @export
#'
#' @examples
#' data(mtcars)
#' obj <- bootstrap(mtcars$cyl)
#' confint(obj)
confint.strop <- function(object, parm, level = 0.95, ...) {
    quantile(object, (1 + c(-1, 1) * level) / 2, ...)
}
