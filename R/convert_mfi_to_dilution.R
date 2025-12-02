#' Convert mfi to dilution using known standard curve fit.
#' @description
#' Convert mfi to dilution using known standard curve fit.
#'
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1.
#' @return Returns the dilution of each sample in mfi.
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is typically used within larger analysis pipelines.
#' # Below is a minimal runnable example using dummy values.
#'
#' # Dummy five-parameter logistic fit parameters:
#' # a, b, c, d, e  (with e on the log scale)
#' # Additional placeholders (f, g) included so params[6] and params[7] exist.
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01),
#'                   f = -5, g = 5)
#'
#' # Example MFI value
#' mfi_example <- 1500
#'
#' # Minimum relative dilution allowed
#' min_rel_dil <- 1
#'
#' # Convert MFI to dilution
#' convert_mfi_to_dilution(mfi_example, dummy_params, min_rel_dil)
convert_mfi_to_dilution <- function(mfi, params, min_relative_dilution) {
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
  result[y < params[6]] <- min_relative_dilution
  result[y > params[7]] <- 1.0
  # I dont think this will happen - Eamon (ask if needed)
  result[result > 1.0] <- 1.0
  return(result)
}
