#' Compute the predictions and confidence intervals
#'
#' Compute predictions from [`fitme`][`spaMM::fitme`] model fit
#' with 95% confidence intervals. It is
#' possible to account for the random effects by integrating predictions over the entire
#' distribution of such effects. In such a case, the predictions produced thus correspond to
#' marginal predictions.
#'
#' We recommend you to look at the raw R code to understand how they work. We commented the code to make this clear.
#' While you could directly look at the code of these functions while using the package, mind that
#' the comments will have been stripped away during the installation process. We kept all the code
#' of each function self contained, instead of writing modular functions to ease the exploration
#' of the code (everything is in one place). One drawback is that it leads to a code full of
#' repetitions.
#'
#' @name predictions
#' @param fit a model fitted (directly or indirectly) with [`fitme`][`spaMM::fitme`] or any [`fit_xx`][`fit_models`] function from this package
#' @param newdata a `data.frame` providing the values for the fixed effect predictors
#' @param random a `logical` indicating whether the predictions should be integrated
#'   over random effect(s)
#' @param nb_boot the number of simulations for the parametric bootstrap used to
#'   compute the intervals
#' @param seed an `integer` providing the seed for the random generator
#' @return a list containing the predictions  and  intervals, as well as the 
#' estimates from each bootstrap replicate used to compute 
#' the intervals and the object created by [`boot.ci`][`boot::boot.ci`].
#' @seealso \code{\link[twinR]{compute_predictions}}
compute_predictions <- function (fit, newdata, random = TRUE, nb_boot = 1000L, seed = 123L) 
{
  set.seed(seed)
  if (nb_boot < 1000L) 
    warnings("The number of bootstrap simulations used is lower than 1000. You should increase 'nb_boot' for reliable results!")
  if (length(fit$lambda) > 2L) 
    stop("This function can only handle up to 2 random effect.")
  
  ## identify the response variable:
  response_var_raw <- paste(fit$call[[2]][[2]])
  if (length(response_var_raw) == 1L) {
    response_var <- response_var_raw
  }
  else if (length(response_var_raw) > 1L) {
    response_var <- response_var_raw[2] ## for binomial binary
  }
  
  ## capture the data from the fitted model:
  data <- fit$data
  
  ## refit the model to avoid scoping issues during bootstraps:
  if (nb_boot > 0L) {
    fit <- spaMM::update.HLfit(fit, data = data)
  }
  
  ## extract the correct inverse link function:
  linkinv <- ifelse(is.null(fit$family$zero_truncated) || 
                      fit$family$zero_truncated == FALSE, function(eta) fit$family$linkinv(eta), 
                    function(eta) fit$family$linkinv(eta, mu_truncated = TRUE)) ## for Tnegbin and Tpoisson
  
  ## define function computing marginal predictions with no random effects variable:
  compute_prediction_0_random <- function(fit, newdata_line) {
    ## we compute the prediction without the random effect on the scale of the link:
    prediction <- spaMM::predict.HLfit(fit, newdata = newdata_line, 
                                       re.form = NA, type = "link")[[1]]
    linkinv(prediction)
  }
  
  ## same thing when there is one random effect:
  compute_prediction_1_random <- function(fit, newdata_line) {
    
    ## we compute the prediction without the random effect on the scale of the link:
    prediction <- spaMM::predict.HLfit(fit, newdata = newdata_line, 
                                       re.form = NA, type = "link")[[1]]
    
    ## we integrate over the entire (Gaussian) distribution of the random effects:
    fn <- function(x) {
      linkinv(prediction + stats::qnorm(x, sd = sqrt(fit$lambda[[1]])))
    }
    stats::integrate(fn, lower = 0, upper = 1)$value
  }
  
  ## same thing when there is one random effect:
  compute_prediction_2_random <- function(fit, newdata_line) {
    
    ## we compute the prediction without the random effect on the scale of the link:
    prediction <- spaMM::predict.HLfit(fit, newdata = newdata_line, 
                                       re.form = NA, type = "link")[[1]]
    
    ## we integrate over the entire (Gaussian) distributions of the random effects;
    ## the package {pracma} is used to perform a bivariate integration:
    fn <- function(x, y) linkinv(prediction + stats::qnorm(x, 
                                                           sd = sqrt(fit$lambda[[1]])) + stats::qnorm(y, sd = sqrt(fit$lambda[[2]])))
    pracma::integral2(fn, xmin = 0, xmax = 1, ymin = 0, 
                      ymax = 1)$Q
  }
  
  ## select the function used to compute the predictions:
  compute_prediction <- switch(as.character(length(fit$lambda)),
                               "1" = compute_prediction_1_random,
                               "2" = compute_prediction_2_random,
                               "0" = compute_prediction_0_random)
  
  ## create wrapper around the correct function that computes all predictions:
  compute_all_predictions <- function(y, ...) {
    
    ## refit model, but only if the response has been simulated during the bootstrap:
    if (!all(y == data[, response_var])) {
      fit <- spaMM::update_resp(fit, newresp = y)
    }
    
    rec <- numeric(nrow(newdata))
    
    draw_progress <- nb_boot == 0L && nrow(newdata) > 10L
    if (draw_progress) 
      pb <- utils::txtProgressBar(max = nrow(newdata), 
                                  width = 100, style = 3) ## add progress bar if big job
    
    ## note: apply() cannot be use as it would coerce types
    for (i in seq_len(nrow(newdata))) { 
      rec[i] <- compute_prediction(fit, newdata[i, , drop = FALSE])
      if (draw_progress) 
        utils::setTxtProgressBar(pb, i)
    }
    rec
  }
  ## single run of the correct function on the original data (for central estimate):
  estimates <- compute_all_predictions(data[, response_var])
  
  ## create robust wrapper around the correct function:
  compute_all_predictions_safe <- function(y, ...) {
    tryCatch(compute_all_predictions(y = y, ...), error = function(e) rep(NA, 
                                                                          length(estimates)))
  }
  ## run the parametric bootstrap (or not depending on nb_boot):
  if (nb_boot == 0L) {
    return(list(results = cbind(newdata, estimates = estimates)))
  } else {
    ## compute the distribution of the focal statistic by bootstrap using {spaMM}:
    boot <- spaMM::spaMM_boot(fit, compute_all_predictions_safe, 
                              nsim = nb_boot, linkinv = linkinv, type = "marginal")
    
    bootreps <- stats::na.omit(boot$bootreps) ## drop failed bootstraps
    
    ## compute the (basic) CI of the expected value of the statistic using {boot}:
    bootCI <- sapply(seq_len(length(estimates)), function(i) {
      boot::boot.ci(boot.out = list(R = nrow(bootreps)), 
                    t0 = estimates[i], t = bootreps[, i], type = "basic")
    }, simplify = FALSE)
    CI <- do.call(rbind, lapply(bootCI, function(i) i$basic[, 
                                                            4:5]))
    
    ## check if some bootstraps failed:
    if (nrow(bootreps) < nb_boot) {
      warning(paste0(nb_boot - nrow(bootreps), " failed bootstraps out of ", 
                     nb_boot))
    }
    ## output:
    return(list(results = cbind(newdata, data.frame(estimate = estimates, 
                                                    lwr = CI[, 1], upr = CI[, 2])), bootreps = boot, 
                bootCI = bootCI))
  }
}


