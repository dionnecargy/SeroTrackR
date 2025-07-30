#' Convert known dilution to mfi from fitted standard curve
#' @description
#' Convert dilution to predicted mfi using known standard curve fit.
#'
#' @param dilution Known dilution of samples
#' @param params Known parameters for five parameter logistic fit.
#' @return Returns the predicted mfi of a sample with known dilution.
#' @export
#' @author Eamon Conway
convert_dilution_to_mfi <- function(dilution, params) {
  if (is.null(dilution) || is.null(params)) {
    error("Require both mfi and params to run.")
  }
  exp(log_logistic_5p(dilution, params[1], params[2], params[3], params[4], exp(params[5])))
}
