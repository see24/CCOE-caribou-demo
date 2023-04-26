#!/bin/bash
#############################################
#####Run R script##################
echo "updating repo code"
cd /Caribou-Demographic-Projection-Paper
git pull https://<GITHUBPAT>@github.com/LandSciTech/Caribou-Demographic-Projection-Paper.git

echo "Running the scripts"
nohup Rscript --vanilla "make.R"
nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTestPar.R" > $AZ_BATCH_TASK_WORKING_DIR/nohup.out 2>&1
