#' Standard Curve for one plate
#'
#' This function is primarily for the species Pk/Pf/Pv analysis to visualise the MFI of standards.
#'
#' @param runPlasmoSeropoint_output Output from `runPlasmoSero5point()` or `runPlasmoSero10point()`
#' @param plate_name  The plate you would like to visualise
#'
#' @returns ggplot of standard curves for each protein and species
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr filter   mutate
#'
#' @author Dionne Argyropoulos
plotOneStdCurve <- function(runPlasmoSeropoint_output, plate_name){

  std_data <- runPlasmoSeropoint_output[[3]] %>%
    dplyr::filter(str_detect(cat, "Standard")) %>%
    dplyr::mutate(std_sample = sub("\\s.*", "", sample_id)) %>%
    dplyr::filter(plate == plate_name)

  num_standards <- dplyr::n_distinct(std_data$std_sample)

  std_data <- std_data %>%
    dplyr::mutate(
      std_sample = factor(
        std_sample,
        levels = if (num_standards == 10) {
          paste0("S", 1:10)
        } else if (num_standards == 5) {
          paste0("S", 1:5)
        } else {
          unique(std_sample)  # fallback just in case
        }
      )
    )

  std_data %>%
    ggplot2::ggplot(aes(x = std_sample, y = mfi, color = species, group = cat)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::labs(x = "Standard Curve", y = "log(MFI)") +
    ggplot2::facet_wrap(species~protein) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
