FROM rocker/r-ubuntu:jammy

RUN apt-get update && apt-get install -y --no-install-recommends \
        git \
	jags

RUN mkdir /home/project

RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
 && R -e "remotes::install_deps(upgrade = 'never')"

COPY Caribou-Demographic-Projection-Paper /home/project
COPY BayesianCaribouDemographicProjection /home/tool

CMD cd /home/project/analysis/scripts/ \
  && nohup Rscript --vanilla sensitivityMinimalTest.R 1 "local_docker" \
