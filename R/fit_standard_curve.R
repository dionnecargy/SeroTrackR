#' Fit a standard curve to known mfi and dilution values.
#' @description
#' We wish to convert the standard curve samples to a five parameter logistic curve.
#' This function takes those values and calls optim to determine the fit.
#'
#' @param mfi Known mfi of samples
#' @param dilution Known dilution of samples
#' @param control Optional list of control parameters for the underlying call to optim.
#'
#' @return standard curve log logistic
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is typically called within data-processing workflows.
#' # Workflow-style example (not run on CRAN)
#'
#' \donttest{
#'
#' # This block demonstrates how fit_standard_curve() is typically used
#' # inside the MFItoRAU_ETH-conversion pipeline.
#'
#' # Step 1 — Prepare master file (normally from readSeroData)
#' master_file <- data.frame(
#'   Location = c("A1","A2","A3"),
#'   Sample   = c("S1","S2","S3"),
#'   Plate    = c("Plate1","Plate1","Plate1"),
#'   Ag1 = c(12000, 8000, 4000),
#'   Ag2 = c(9000,  5000, 2500)
#' )
#'
#' # Convert antigen columns to numeric
#' L <- master_file |>
#'   dplyr::mutate(dplyr::across(-c(Location, Sample, Plate), as.numeric))
#'
#' # Fake plate layout (normally from readPlateLayout)
#' layout <- list(Plate1 = data.frame(Location = c("A1","A2","A3"), WellType = "STD"))
#'
#'
#' # Step 2 — Load reference standard curve MFI values (dummy data)
#' refs <- data.frame(
#'   std_plate = rep("StdPlate1", 5),
#'   antigen   = rep("Ag1", 5),
#'   dilution  = c(1, 1/2, 1/4, 1/8, 1/16),
#'   eth_mfi   = c(14000, 7000, 3500, 1800, 900),
#'   png_mfi   = c(15000, 7600, 3800, 1900, 950)
#' )
#'
#'
#' # Step 3 — Define optimisation settings
#' control <- list(
#'   maxit  = 10000,
#'   abstol = 1e-8,
#'   reltol = 1e-6
#' )
#'
#'
#' # Step 4 — Fit ETH and PNG curves per standard-plate × antigen
#' ref_fit <- refs |>
#'   dplyr::group_by(.data$std_plate, .data$antigen) |>
#'   tidyr::nest() |>
#'   dplyr::mutate(
#'     eth_fit = purrr::map(data, ~ fit_standard_curve(.x$eth_mfi, .x$dilution, control)),
#'     png_fit = purrr::map(data, ~ fit_standard_curve(.x$png_mfi, .x$dilution, control))
#'   )
#'
#' ref_fit
#' }
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
