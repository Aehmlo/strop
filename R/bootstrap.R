#' Bootstrap constructor
#'
#' @param data The data from which to resample
#' @param FUN The function of interest
#' @param n The number of resamples to perform
#' @param ... Additional arguments
#'
#' @return A an object with class \code{strop}
#' @export
#'
#' @examples
#' data(mtcars)
#' bootstrap(mtcars$cyl)
bootstrap <- function(data, FUN = mean, n = 1000, ...) {
    call <- match.call()
    fn <- match.fun(FUN)
    pop <- fn(data)
    samples <- matrix(sample(data, n * length(data), replace = T), ncol = n)
    stats <- apply(samples, 2, FUN)
    ret <- list(
        call = call,
        pop = pop,
        samples = samples,
        stats = stats
    )
    class(ret) <- append(class(ret), "strop")
    invisible(ret)
}
