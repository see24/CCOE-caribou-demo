#!/bin/bash
#############################################
#####installing git and jags onto node##################
apt-get update
apt-get install -y --no-install-recommends git-all
apt-get install -y --no-install-recommends jags
echo "Finished installing Git, jags, and updating Node"

#############################################
#####clone repo to get code needed##################
git clone https://[PAT]@github.com/LandSciTech/Caribou-Demographic-Projection-Paper.git
git clone https://[PAT]@github.com/LandSciTech/BayesianCaribouDemographicProjection.git

mkdir R-packages
export R_LIBS=R-packages
#############################################
#####Run R script##################
echo "Running the scripts"
Rscript "Caribou-Demographic-Projection-Paper/make.R"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 1 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 2 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 3 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 4 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 5 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 6 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 7 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 8 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 9 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 10 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 11 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 12 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 13 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 14 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 15 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 16 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 17 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 18 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 19 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 20 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 21 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 22 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 23 "cloud"
nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" 24 "cloud"