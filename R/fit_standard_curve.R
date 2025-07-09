#' Fit a standard curve to known mfi and dilution values.
#' @description
#' We wish to convert the standard curve samples to a five parameter logistic curve.
#' This function takes those values and calls optim to determine the fit.
#'
#' @param mfi Known mfi of samples
#' @param dilution Known dilution of samples
#' @param control Optional list of control parameters for the underlying call to optim.
#' @export
#' @author Eamon Conway
fit_standard_curve <- function(mfi, dilution, control = NULL) {
  if (is.null(mfi) | is.null(dilution)) {
    error("Require both mfi and dilution to run.")
  }

  y1 <- log(mfi)
  initial_solution <- c(-1.0, 0.0, max(y1), 0.0, 0.0)

  error_func <- function(x) {
    f1 <- log_logistic_5p(dilution, x[1], x[2], x[3], x[4], exp(x[5]))
    sum((y1 - f1)^2.0)
  }

  solution <- optim(par = initial_solution, fn = error_func, control = control)
  if (solution$convergence != 0) {
    stop("Standard curve failed to converge. Look at data and possibly change control parameters from default.")
  }
  c(solution$par, min(y1), max(y1))
}

inverse_log_logistic_5p <- function(y,b,c,d,e,f){
  A <- (d/(y-c))^(1/f)-1
  return(exp(-e) *A^(1/b))
}

log_logistic_5p <- function(x, b, c, d, e, f) {
  return(c + d / (1.0 + exp(b * (log(x) + e)))^f)
}
