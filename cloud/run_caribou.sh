#!/bin/bash
#############################################
#####Run R script##################
echo "updating repo code"
cd /CCOE-caribou-demo
git pull https://<PAT>@github.com/LandSciTech/CCOE-caribou-demo.git

echo "Running the scripts"
nohup Rscript --vanilla "analysis/scripts/sensitivityMinimal.R" <batch> > $AZ_BATCH_TASK_WORKING_DIR/nohup_<batch>.out 2>&1
