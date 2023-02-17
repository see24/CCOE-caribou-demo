#' Caribou-Demographic-Projection-Paper: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Sarah Endicott \email{sarah.endicott@canada.ca}
#'
#' @date 2023/02/16


install.packages("devtools", dependencies = TRUE)
## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps("Caribou-Demographic-Projection-Paper", upgrade = "never", lib = "R-packages")

devtools::install_github("LandSciTech/caribouMetrics", lib = "R-packages")

## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))
