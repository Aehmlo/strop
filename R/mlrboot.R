#' Multiple linear regression bootstrapping
#'
#' @param fit A fitted model (e.g. \code{lm}, \code{lm}) object
#' @param ... Additional parameters
#'
#' @return An object of class \code{strop}
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- lm(cyl ~ ., mtcars)
#' mlrboot(model)
mlrboot <- function(fit, ...) {
    betas <- function(data) {
        model <- update(fit, data = data)
        coef(model)
    }
    bootstrap(fit$model, betas)
}
