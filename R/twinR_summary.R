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
#' @seealso code{\link[twinR]{build_fit_summary.table}}
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

#' Create data summary tables
#'
#' This function is used to create the tables providing the details of the data used in the study.
#'
#' @param birth_level_data the dataset to summarize
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' build_data_summary.table(data_births_all)
#' @seealso \code{\link[twinR]{build_data_summary.table }}
build_data_summary.table <- function(birth_level_data) {
  se <- function(x){
    sd(x, na.rm = T)/sqrt(length(x))
  }
  ## add total births and twinning status to the data:
  birth_level_data |>
    dplyr::group_by(.data$pop, .data$maternal_id) |>
    dplyr::mutate(total_births = dplyr::n(),
                  twinners     = any(.data$twin),
                  afb = min(.data$maternal_age)/12,
                  nontwinners  = all(!.data$twin)) |>
    dplyr::ungroup() -> data
  
  ## notes for ASCII encoding of unicodes:
  # - a umlaut  = \u00e4
  # - o umlaut  = \u00f6
  # - o with stroke = \u00f8
  # - oe        = \u0153
  # - per mille = \u2030
  
  ## build the part of the table that contains information for all populations combined:
  tibble::tibble("Populations" = paste0(unique(data$pop), collapse = ", "),
                 "Dataset" = "",
                 "Maternal birth period" = paste(min(floor(data$maternal_birthyear), na.rm = TRUE), "-", max(floor(data$maternal_birthyear), na.rm = TRUE), sep = ""),
                 "1/4 and 3/4 quantiles for maternal birth period" = paste(quantile(data$maternal_birthyear, 1/4, na.rm = TRUE),"-", quantile(data$maternal_birthyear, 3/4, na.rm = TRUE), sep = ""),
                 "Age at first birth (mean-SE) " =  paste(round(base::mean(data$afb, na.rm = TRUE), digits = 2L), "-", signif(se(data$afb),2), sep = ""),
                 "Mothers" = length(unique(data$maternal_id)),
                 "Non-twinners" = length(unique(data$maternal_id[data$nontwinners])),
                 "Twinners" = length(unique(data$maternal_id[data$twinners])),
                 "Twinner rate (\u2030)" = round(.data$Twinners / (.data$Twinners + .data$`Non-twinners`) * 1000, digits = 2L),
                 "Offspring birth period" = paste(min(data$birth_year, na.rm = TRUE), "-", max(data$birth_year, na.rm = TRUE), sep = ""),
                 "1/4 and 3/4 quantiles for offspring birth period" = paste(quantile(data$birth_year, 1/4, na.rm = TRUE),"-", quantile(data$birth_year, 3/4, na.rm = TRUE), sep = ""),
                 "Births" = nrow(data),
                 "Singleton births" = sum(!data$twin, na.rm = TRUE),
                 "Twin births" = sum(data$twin, na.rm = TRUE),
                 "Twinning rate (\u2030)" = round(mean(data$twin, na.rm = TRUE) * 1000, digits = 2L),
                 "Total births (min-median-max)" =  paste(1, "-", round(stats::median(data$total_births, na.rm = TRUE), digits = 2L), "-", max(data$total_births, na.rm = TRUE), sep = ""),
                 "Total births (mean-SE)" =  paste(round(base::mean(data$total_births, na.rm = TRUE), digits = 2L), "-",  signif(se(data$total_births), 2), sep = "")
                 ) -> tbl_total
  

  tbl_total
}

