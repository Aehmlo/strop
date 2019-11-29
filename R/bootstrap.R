#' Bootstrap constructor
#'
#' @param data The data from which to resample
#' @param FUN The function of interest
#' @param n The number of resamples to perform
#' @param ... Additional arguments
#'
#' @return A bootstrap object of class \code{strop}
#' @export
#'
bootstrap <- function(data, FUN, n, ...) UseMethod("bootstrap")

#' #' Bootstrap constructor
#'
#' @param data The (atomic) data from which to resample
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
bootstrap.default <- function(data, FUN = mean, n = 1000, ...) {
    call <- match.call()
    fn <- match.fun(FUN)
    pop <- fn(data)
    samples <- matrix(sample(data, n * length(data), replace = T), ncol = n)
    stats <- sapply(samples, FUN)
    invisible(structure(class = "strop", list(
        call = call,
        pop = pop,
        samples = samples,
        stats = stats
    )))
}

#' Bootstrap constructor
#'
#' @param data The data frame from which to resample
#' @param FUN The function of interest
#' @param n The number of resamples to perform
#' @param ... Additional arguments
#'
#' @importFrom purrr transpose
#'
#' @return A an object with class \code{strop}
#' @export
#'
#' @examples
#' data(mtcars)
#' bootstrap(mtcars, function(df) mean(df$cyl))
bootstrap.data.frame <- function(data, FUN = mean, n = 1000, ...) {
    call <- match.call()
    fn <- match.fun(FUN)
    pop <- fn(data)
    samples <- lapply(1:n, function(.i) {
        data[sample(1:nrow(data), nrow(data), replace = T),]
    })
    stats <- transpose(lapply(samples, fn))
    invisible(structure(class = "strop", list(
        call = call,
        pop = pop,
        samples = samples,
        stats = stats
    )))
}
