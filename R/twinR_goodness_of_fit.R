#' Perform the goodness of fit test of simulation scenario(s)
#'
#' This functions perform the goodness of fit test on the data created by the functions
#' [`simulate_slopes_for_GOF`] or [`combine_simulated_slopes`]. The justification and formal
#' description of the test are given in the Appendix S1 of our Supplementary Material.
#'
#' @param slopes_obj an object returned by [`simulate_slopes_for_GOF`] or [`combine_simulated_slopes`]
#'
#' @return a tibble with the information about the scenario and the computed p-values
#' @export
#' @seealso [`simulate_slopes_for_GOF`], [`combine_simulated_slopes`]
#' @examples
#' # See ?twinR
#'
goodness_of_fit <- function(slopes_obj) {
  
  ## recursive call if the list contains multiple scenarios:
  if (length(unique(slopes_obj$scenario)) > 1L) {
    slopes_obj |>
      dplyr::mutate(scenario = factor(.data$scenario, levels = c("P", "I", "S", "H", "PI", "PS", "PH", "IS", "IH", "SH", "PIS", "PIH", "PSH", "ISH", "PISH", "base_model"))) |>
      dplyr::group_split(.data$scenario) -> list_slopes_obj
    
    all_gof <- lapply(list_slopes_obj, goodness_of_fit)
    return(do.call("rbind", all_gof))
  }
  
  ## we do not need the individual slopes for the 2nd level of bootstrapping but only their means,
  ## so we aggregate the data:
  slopes_obj |>
    dplyr::group_by(.data$scenario, .data$seed) |>
    dplyr::summarize(
      slope_observed = unique(.data$slope_observed),
      slopes_level1 = unique(.data$slopes_level1),
      mean_slopes_level2 = mean(.data$slopes_level2), .groups = "keep") |>
    dplyr::ungroup() -> slopes_obj_aggregated
  
  
  ## fit a model predicting slopes of first bootstrap based on the slopes at second bootstrap:
  fit <- stats::lm(slopes_level1 ~ mean_slopes_level2, data = slopes_obj_aggregated)
  
  ## compute residuals
  var_error <- summary(fit)$sigma
  residuals_slopes_level1 <- var_error*stats::rstudent(fit) # gives similar results than simply using residuals(fit) but more theoretically sound
  
  ## predict bias introduced by bootstrapping (based on the difference between the two levels):
  mean_slopes_level1 <- mean(slopes_obj_aggregated$slopes_level1)
  bootstrap_bias <- stats::predict(fit, newdata = data.frame(mean_slopes_level2 = mean_slopes_level1))[[1]]
  
  ## remove bias to observed slope:
  slope_observed <- unique(slopes_obj_aggregated$slope_observed)
  slope_observed_unbiased <- slope_observed - bootstrap_bias
  
  ## compute p-value (unilateral -> following the idea of the flower plot: we aim at predicting a poistive slope, so scenario producing large negative plots should be rejected)
  pv_gof <- (1 + sum(residuals_slopes_level1 > slope_observed_unbiased)) / (1 + nrow(slopes_obj_aggregated))
  
  ## raw computation of p-value based on single bootstrap only (for comparison):
  pv_raw <- (1 + sum(slopes_obj_aggregated$slopes_level1 > slope_observed)) / (1 + nrow(slopes_obj_aggregated))
  
  ## output:
  tibble::tibble(scenario = as.character(unique(slopes_obj_aggregated$scenario)), pv_gof = pv_gof, pv_raw = pv_raw)
  
}