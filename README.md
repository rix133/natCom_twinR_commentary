# Commentary to twinR 

This is the repository providing the code and the data associated with the commentary on the paper
"[**Mothers with higher twinning propensity had lower fertility in pre-industrial Europe**](https://doi.org/10.1038/s41467-022-30366-9)" (Nature Communications, 2022) 


## Running Code

Running code from this package expects that you have R 4.1.0 or later installed. 

First, make sure the [twinR package](https://github.com/courtiol/twinR) and its dependencies are installed. First install the package **{remotes}** (if it is not already installed on your system), then simply type the following in your R Console:

```r
remotes::install_github("courtiol/twinR", dependencies = TRUE)
```

Then, you can clone this repository and run the code from the Rmarkdown document "[S1_replicate_stats_and_plots.Rmd](https://github.com/rix133/natCom_twinR_commentary/blob/main/S1_replicate_stats_and_plots.Rmd)" line by line using RStudio to reproduce the analyses. If you wish to convert this into R script to run from console you can e.g. use:

```r
knitr::purl("S1_replicate_stats_and_plots.Rmd", output="S1_replicate_stats_and_plots.R")
```

## Running simulations

Its suggested to use a linux computing cluster to run the simulation scenarios one by one (using *run_simulation_EE_not_last.R*) rather than running the file *S3_run_simulations_EE_not_last.Rmd*. E.g. do it like:

```bash 
Rscript run_simulation_EE_not_last.R --model H --cores 51
```
The memory (RAM) usage is quite high for simulations. The total number of CPUs used for running single simulation on Estonian data w/o last birth was 51. RAM usage varied between models but stayed around 18 GB per CPU core (dumped cores had a size range from 12 to 23GB).

An example SLURM script to run PIS scenario in a cluster would hence be

```bash 
#!/bin/sh
#SBATCH --job-name=PIS_sim_twinR_EE_not_last
#SBATCH --time=12:10:00
#SBATCH --mem=990G
#SBATCH --cpus-per-task=51
#SBATCH --partition=main


scenario=PIS

project_name=natCom_twinR_commentary

BASEDIR=./projects/$project_name

# load required modules (ie. basically R > 4.1.0) with all dependencies installed
module load any/R/4.1.2-X


#run a script for a single Scenario
Rscript run_simulation_EE_not_last.R --model $scenario --cores $SLURM_CPUS_PER_TASK

```


## Changes from twinR Package Functions

This repository has made few small fixes to 2 functions in twinR package in order to be run on different environments:

* pipe operator (%>%) from tidyverse has been replace by base R pipe operator (|>) 
* cAIC calculation has been omitted from model summary table
* fit summary table has been refactored to run seamlessly with 0 lambdas
* Remove population from all models 
* adding optional parameter *extraVars* to models that is appended to the formula (e.g to add pop do `extraVars="+(1|pop)"`)

Also a function has been added to extract the last birth for each mother as well as some other convenience functions.

