#' Compute R-squared for GLM
#'
#' This function computes the R-squared value for a fitted GLM model.
#' @param m3 A fitted GLM model object.
#' @param printIt Logical value indicating whether to print the computed
#'  R-squared value. Default is TRUE.
#' @return The computed R-squared value.
glmR2 <- function(m3, printIt=T){
  r2 <- with(summary(m3), 1 - deviance/null.deviance)
  if(printIt){
    cat(paste("R2=",round(r2, 3)))
  }
  invisible(r2)
}

#' Convert Formula to String
#'
#' This function converts a formula object into a string representation.
#' @param formula A formula object to be converted into a string.
#' @return A string representation of the input formula.
form2str <- function(formula){
  fstr <- paste0(as.character(formula)[2:3], collapse = "~")
  fstr <- gsub(" ", "", fstr)
  #remove characters not allowed in Windows file names
  fstr <- gsub("[<>:\"/\\\\|?*]", "_x_", fstr)
  fstr
}