#' Fit Predictions
#'
#' This function fits a model using the given formula and dataset and 
#' computes predictions. The model is fit using the \code{\link[spaMM]{fitme}}
#' function from the \strong{spaMM} package.
#' 
#' @param dataset A data frame containing the data to be used for fitting the model.
#' @param formula A formula specifying the model to be fit.
#' @param predict Logical value indicating whether to do predictions. Default is TRUE.
#' @param nb_boot Number of bootstrap samples to use when computing predictions.
#'  Default is 1000.
#' @param save Logical value indicating whether to save model fit and predictions
#'  to file. Useful if running same fits several time  Default is TRUE.
#' @param saveDir Directory where precomputed predictions and fits are stored.
#'  Default is "./data/predictions".
#' 
#' @return A list containing the fitted model object and a data frame with
#'  computed predictions.
#' @seealso \code{\link[twinR]}
fitPredictions <- function(dataset, formula,
                           predict=TRUE,
                           nb_boot=1000,
                           save=TRUE,
                           saveDir = "./data/predictions"){
  
  if(!dir.exists(saveDir)) dir.create(saveDir)
  
  
  fitName <- deparse(substitute(dataset))
  # add formula also to the fitName like:
  fitName <- paste0(form2str(stats::as.formula(formula)),fitName)
  
  args <- list(formula = stats::as.formula(formula),
               data = dataset, 
               family = stats::binomial(link = "logit"),
               method = "PQL/L")
  
  fitDataFname <- paste0(saveDir,"/",fitName ,"_fit.rds")
  if(save){
    if(!file.exists(fitDataFname)){
      fit <-  twinR::fit_model_safely(timeout = Inf, .args = args)
      saveRDS(fit, fitDataFname)
    } else{
      warning("Pre-computed fit returned from file:\n", fitDataFname,
              "\n If you want to re-run this step delete the file",
              " or change the saveDir!\n")
      fit <- readRDS(fitDataFname)
    }
  } else {
    fit <-  twinR::fit_model_safely(timeout = Inf, .args = args)
  }
  
  #garbage collecion after spaMM multi-core proccess
  gcstuff <- gc(verbose=FALSE)
  
  get_predictions <- function(predDataFname, fit, dataset, args, save){
    if(!file.exists(predDataFname)){
      nd <- NULL
      
      # mother level data
      if("births_total" %in% all.vars(args$formula)){
        min_births <- min(dataset$births_total)
        max_births <- max(dataset$births_total)
        nd <- data.frame(births_total = min_births:max_births)
      }
      #birth level data
      if("age" %in% all.vars(args$formula) & "parity" %in% all.vars(args$formula)){
        nd <- twinR::prepare_newdata_fig_3(dataset, xaxis = "age")
        ## remove the twin covariate, which is not used in twin_fit:
        nd  <-  nd[nd$twin,] 
        nd$twin <- NULL
      }
      
      if(!is.null(nd)){
        data_fig <- compute_predictions(fit,
                                        newdata = nd,
                                        nb_boot = nb_boot)
        
        if(save){
          saveRDS(data_fig, predDataFname)
        }
        
        
      } else {
        warning("TODO! New data for predictions not set, skipping!")
        return(list(results = NULL))
      }
      
      
    } else {
        warning("Pre-computed predictions returned from file:\n", predDataFname,
                "\n If you want to re-run this step delete the file",
                " or change the saveDir!\n")
        data_fig <- readRDS(predDataFname)
    }
      
    #garbage collecion after spaMM multi-core proccess
    gcstuff <- gc(verbose=FALSE)
    return(data_fig)
  }
    
    
    
  
  predDataFname <- paste0(saveDir,"/",fitName ,"data_fig.rds")
  if(predict){
      data_fig <- get_predictions(predDataFname, fit, dataset, args, save)
    } else {
      data_fig <- list(results = NULL)
    }
    
  
  return(list(fit=fit, results=data_fig$results))
}