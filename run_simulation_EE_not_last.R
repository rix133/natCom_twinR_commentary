## read the model letter to simulate  and nr cores from commandArgs

args <- commandArgs(trailingOnly = TRUE)
model <- NULL
cores <- NULL
refit <- FALSE

if(!"--model" %in% args){stop("Argument --model missing with no default!")}
if(!"--cores" %in% args){stop("Argument --cores missing with no default!")}

scenarios_to_do <- c("base_model", "P", "I", "S", "H",
                     "PI", "PS", "PH", "IS", "IH", "SH",
                     "PIS", "PIH", "PSH", "ISH", "PISH")

model_arg <- match.arg("--model", args)
if (length(model_arg) > 0) {
  model_index <- which(args == model_arg)
  model <- args[model_index + 1]
  if(!model %in% scenarios_to_do){
    stop("--model has undefined scenario ", model, ". Accepted values include ",
         paste0(scenarios_to_do, collapse = ", "))
  }
  scenario <- model
}

cores_arg <- match.arg("--cores", args)
if (length(cores_arg) > 0) {
  cores_index <- which(args == cores_arg)
  cores <- as.integer(args[cores_index + 1]) - 1
  if(is.na(cores) | is.null(cores)){stop("--cores must be a number")}
}

## ------------------------------------------------------------------------------------------------------------------------------
#get last birth adding function
source("./R/last_birth.R")

#simplified twinR summary tables 
source("./R/twinR_summary.R")

#fix twinR compute predictions to do prediction with no lambda  as well
source("./R/twinR_predictions.R")

#simple convenience functions
source("./R/utils.R")




## ------------------------------------------------------------------------------------------------------------------------------
## Identify number of CPU cores available for parallel computing,
## note: using a large number may lead RAM to max out, so you may have to adjust
## that according to your infrastructure:
nb_cores <- cores

#it seems that simulating the base model for Estonian not last birth data
#requires about 15GB of RAM per CPU core (tested with 61 CPU cores)


## Set option in spaMM:
spaMM::spaMM.options(nb_cores = nb_cores)


## ------------------------------------------------------------------------------------------------------------------------------
#Import and preproccess Estonian Data

data_births_monthly_EE <- readRDS("./data/data_births_all_EE.rds")

#the twinR package expects population to be present
data_births_monthly_EE$pop <- "Estonia" 

data_births_monthly_EE <- add_last_birth(data_births_monthly_EE)

data_births_monthly_EE_not_last <- data_births_monthly_EE[!data_births_monthly_EE$last,]



## ------------------------------------------------------------------------------------------------------------------------------
# import the function to do model fit and predictions
source("./R/fit_models.R")


## ------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#-------------------------------- Goodness-of-fit tests -----------------------------------------
#------------------------------------------------------------------------------------------------

### IMPORTANT: while all the steps above should work no matter the operating system and whether
### you are using R within a GUI (e.g. RStudio) or not, the following step is very
### computationally intensive and has been tailored to be used on a Unix system (e.g. Linux) and
### directly within a terminal. It may work in RStudio, but this is not guaranteed, nor
### recommended. If you run this code using Windows, it should fallback to running the
### computation sequentially instead of in parallel across multiple CPU cores, which should work
### fine at the cost of requiring probably weeks of running time.

### Run scenarios one by one and save the output in a rda file:

library(twinR)
library(doSNOW)
#it seems that simulating the base model for Estonian not last birth data
#requires about 15GB of RAM per CPU core (tested with 61 CPU cores)

timeout <- 1 * 60 * 60 # an Hour

baseSlopeDir <- "./exports/slopes_under_scenarios"
baseFitDir <- "./exports/fitted_models"


## ------------------------------------------------------------------------------------------------------------------------------
#use models without populations predictor
source("./R/twinR_models.R")


## ----make directories----------------------------------------------------------------------------------------------------------
expName <- "_EE_nl"
slopeDir <- paste0(baseSlopeDir, expName)
fitDir <- paste0(baseFitDir, expName)

dir.create(slopeDir, recursive = T)
dir.create(fitDir, recursive = T)

data_births_monthly <- data_births_monthly_EE_not_last

#remove unnecessary data frames
rm(data_births_monthly_EE, data_births_monthly_EE_not_last)

cat("Fits are saved in: ", fitDir)


## ----fit model----------------------------------------------------------------------------------------------------------------
## fit all trios of models in parallel (for linux only, but easy to adjust for other OS):
targetFname <-  paste0(fitDir,"/fits_", scenario, "_obs.rda")
  if(!file.exists(targetFname) & refit){
    fits <- fit_life_histories(scenario = scenario,
                             birth_level_data = data_births_monthly)
    save(fits, file =targetFname, compress = "xz")
  
    rm(fits)
  }
  



## ----run scenarios-------------------------------------------------------------------------------------------------------------

  
name_obj <- paste0("slopes_under_", scenario)
targetFname <- paste0(slopeDir,"/", name_obj, ".rda")
if(!file.exists(targetFname)){
  load(file = paste0(fitDir,"/fits_", scenario, "_obs.rda"))
  slopes_under_scenario <- simulate_slopes_for_GOF(N_replicates_level1 = 200L,
                                                   N_replicates_level2 = 20L,
                                                   birth_level_data = data_births_monthly,
                                                   life_history_fits = fits,
                                                   scenario = scenario,
                                                   nb_cores = nb_cores,
                                                   timeout = timeout,
                                                   .log = TRUE, lapply_pkg = "pbmcapply",
                                                   verbose = list(fit = TRUE, simu = FALSE))
     assign(name_obj, value = slopes_under_scenario)
     save(list = name_obj, file = targetFname)
   }
   
cat("Done!")



## ------------------------------------------------------------------------------------------------------------------------------
#END

