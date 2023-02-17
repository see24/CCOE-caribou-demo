#' Caribou-Demographic-Projection-Paper: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Sarah Endicott \email{sarah.endicott@canada.ca}
#'
#' @date 2023/02/16

# I copied these from the rocker/r-bspm Dockerfile because it sets them in the
# RProfile but it wasn't working
bspm::enable()
options(pkgType="binary", install.packages.check.source = "no")

utils::install.packages("devtools", dependencies = TRUE)
## Install Dependencies (listed in DESCRIPTION) ----
print("install deps")
try(devtools::install_deps("Caribou-Demographic-Projection-Paper", upgrade = "never"))

message("install caribouMetrics from GitHub")
try(devtools::install_github("LandSciTech/caribouMetrics",
                             type = "source", dependencies = FALSE))

# do this separately so they can be installed from binaries
try(devtools::install_deps(file.path(.libPaths(), "caribouMetrics")))

## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))
