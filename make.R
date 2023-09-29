#' Caribou-Demographic-Projection-Paper: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Sarah Endicott \email{sarah.endicott@canada.ca}
#'
#' @date 2023/02/16


utils::install.packages("remotes", dependencies = TRUE)
## Install Dependencies (listed in DESCRIPTION) ----
print("install deps")
remotes::install_deps(".", upgrade = "never")

message("install caribouMetrics from GitHub")
remotes::install_github("LandSciTech/caribouMetrics")

