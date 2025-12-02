#' Convert mfi to dilution using known standard curve fit and no lower bound
#' @description
#' Convert mfi to dilution using known standard curve fit and no lower bound unless you are below the asymptote of the standard curve.
#' In this situation we set your value to min_relative_dilution. I dunno argue?
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1.
#' @return Returns the dilution of each sample in mfi.
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is usually called inside higher-level analysis steps.
#' # Below is a minimal runnable example using dummy values.
#'
#' # Dummy five-parameter logistic fit parameters:
#' # a, b, c, d, e  (with e typically on the log scale)
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01), f = 0, g = 5)
#'
#' # Example MFI value
#' mfi_example <- 1500
#'
#' # Minimum relative dilution from the standard curve
#' min_rel_dil <- 1
#'
#' # Convert MFI to dilution without applying a lower bound
#' convert_mfi_to_dilution_no_lower_bound(mfi_example, dummy_params, min_rel_dil)
convert_mfi_to_dilution_no_lower_bound <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  result <- inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y > (params[2] + params[3])] <- 1.0
  result[y < params[2]] <- min_relative_dilution
  result[y > params[7]] <- 1.0
  # I dont think this will happen - Eamon (ask if needed)
  result[result > 1.0] <- 1.0
  return(result)
}
