#' Convert mfi to dilution using known standard curve fit and no bounds
#' @description
#' Convert mfi to dilution using known standard curve fit and no bounds unless you are below the asymptote of the standard curve.
#' In this situation we set your value to min_relative_dilution. I dunno argue?
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1.
#' @return Returns the dilution of each sample in mfi.
#' @export
#' @author Eamon Conway
convert_mfi_to_dilution_no_bounds <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  y[y >= (params[2] + params[3])] <- 0.999*(params[2] + params[3])
  result <- inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y < params[2]] <- min_relative_dilution
  return(result)
}
