#' Convert known dilution to mfi from fitted standard curve
#' @description
#' Convert dilution to predicted mfi using known standard curve fit.
#'
#' @param dilution Known dilution of samples
#' @param params Known parameters for five parameter logistic fit.
#' @return Returns the predicted mfi of a sample with known dilution.
#' @export
#' @author Eamon Conway
#'
#' @examples
#' # This function is typically called internally by higher-level workflows.
#' # Below is a minimal runnable example using dummy parameters.
#'
#' # Five-parameter logistic model typically expects parameters in the order:
#' # a, b, c, d, e  (e often log-transformed)
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01))
#'
#' # Example dilution value
#' dilution_example <- 0.1
#'
#' # Predict MFI from the dummy standard curve
#' convert_dilution_to_mfi(dilution_example, dummy_params)
#'
convert_dilution_to_mfi <- function(dilution, params) {
  if (is.null(dilution) || is.null(params)) {
    error("Require both mfi and params to run.")
  }
  exp(log_logistic_5p(dilution, params[1], params[2], params[3], params[4], exp(params[5])))
}
