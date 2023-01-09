FROM rocker/r-ubuntu:jammy

RUN apt-get update && apt-get install -y --no-install-recommends \
        git \
	jags 

RUN Rscript -e 'install.packages(c("rjags"), repos = "https://cloud.r-project.org")'


