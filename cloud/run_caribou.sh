#!/bin/bash
#############################################
#####installing git and jags onto node##################
apt-get update
apt-get install -y --no-install-recommends git-all
apt-get install -y --no-install-recommends jags
echo "Finished installing Git, jags, and updating Node"

#############################################
#####clone repo to get code needed##################
git clone https://<PAT>@github.com/LandSciTech/Caribou-Demographic-Projection-Paper.git
git clone https://<PAT>@github.com/LandSciTech/BayesianCaribouDemographicProjection.git

mkdir R-packages
export R_LIBS=R-packages

mv "/usr/local/lib/R/site-library/bspm" "/usr/lib/R/library"

#############################################
#####Run R script##################

echo "Running the scripts"
nohup Rscript --vanilla "Caribou-Demographic-Projection-Paper/make.R"
for i in 15 16 18 19 20 21 24
do
	echo "Running batch" $i
	nohup Rscript --vanilla "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" $i "cloud" > nohup_$i.out 2>&1 &
done
wait
