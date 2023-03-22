#' Create fit summary tables
#'
#' This function is used to create the tables providing the details of the fitted models used in the study.
#'
#' @param fit the output of one of the function fitting models (see [`fit_models`])
#'
#' @return a `tibble` with an additional attribute called "formula"
#' @export
#'
#' @examples
#' #See ?twinR
#'
build_fit_summary.table <- function(fit) {

  ## extract raw model output:
  utils::capture.output(fit_components <- spaMM::summary.HLfit(fit))

  ## extract model formula:
  model_formula <- paste(as.character(fit$call$formula)[c(2,1,3)], collapse = " ")

  ## extract and format info on fixed effects:
  fixed_effects <- tibble::as_tibble(fit_components$beta_table) |>
                   dplyr::rename("value" = .data$Estimate) |>
                   dplyr::mutate(name = rownames(fit_components$beta_table), .before = 1)
  fixed_effects <- dplyr::bind_cols(object = c("fixed effects", rep("", max(c(0, nrow(fit_components$beta_table) - 1)))), fixed_effects)

  
  ## extract and format info on random effects:
  random_effects <- data.frame(object=character(0),name=character(0))
  if(!is.null(fit_components$lambda_table)){
    random_effects <- tibble::as_tibble(fit_components$lambda_table) |> 
      dplyr::select(name = "Group") |> 
      dplyr::mutate(name = ifelse("name" == "maternal_.", "maternal_id", "name"),
                    name = paste0("variance between ",  "name"))
    random_effects <- random_effects  |>
      dplyr::bind_cols(object = c("random effects",  rep("", max(c(0, nrow(random_effects) - 1)))))
  }
  
  if (!is.null(fit$lambda)) {
    random_effects <- random_effects |> dplyr::mutate(value = fit$lambda)
  }

  ## extract and format info on model family:
  tibble::tibble(object = "response family",
                 name = as.character(fit$family$family),
                 value = NA) |>
    dplyr::mutate(name = ifelse(.data$name == "negbin", "negative binomial", .data$name),
                  name = paste(.data$name, "with", as.character(fit$family$link), "link")) ->  model_family

  if (fit$family$family == "negbin") {
    model_family |>
      dplyr::mutate(name = paste0(ifelse(fit$family$zero_truncated, "truncated ", ""), .data$name)) |>
      dplyr::bind_rows(tibble::tibble(object = "", name = "shape parameter",
                                      value = get("shape", envir = environment(fit$family$aic)))) -> model_family
  }

  ## extract and format info on number of parameters, likelihood, AICs:
  if(nrow(random_effects)>0){
    model_stats <- dplyr::bind_cols(K = nrow(fixed_effects) + 
                                      nrow(random_effects) + nrow(model_family) - 1, L = tibble::as_tibble_row(fit_components$likelihoods)[2][[1]])
    
  }
  if(nrow(random_effects)==0){
    model_stats <- dplyr::bind_cols(K = nrow(fixed_effects) + 
                                      nrow(random_effects) + nrow(model_family) - 1, L = tibble::as_tibble_row(fit_components$likelihoods)[1][[1]])
    
  }

  AICs <- tryCatch(c(AIC=stats::AIC(fit, verbose = F, also_cAIC = FALSE)[1],cAIC=NA), error=function(e){return(c(AIC=NA,cAIC=NA))})
  #utils::capture.output(AICs <- stats::AIC(fit, verbose = FALSE, also_cAIC = TRUE)[1:2])

  model_stats <- dplyr::bind_cols(model_stats, tibble::as_tibble_row(AICs))
  names(model_stats) <- c("number of model parameters", "marginal log Likelihood", "marginal AIC", "conditional AIC (cAIC)")
  model_stats <- dplyr::bind_cols(object = c("fit info", rep("", ncol(model_stats) - 1)),
                                  tidyr::pivot_longer(cols = 1:ncol(model_stats), model_stats))

  ## extract and format info on fitted data:
  tibble::tibble(object = "data info", name = "number of fitted observations (N)", value = nrow(fit$data)) -> fitted_data

  ## combine all formated info:
  dplyr::bind_rows(fixed_effects, random_effects, model_family, model_stats, fitted_data) |>
    dplyr::rename(Type = .data$object,
                  "Variable" = .data$name,
                  Value = .data$value) -> all_results

  ## add formula as an atribute of the tibble:
  attr(all_results, "formula") <- model_formula

  all_results
}
