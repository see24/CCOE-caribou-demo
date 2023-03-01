FROM rocker/r-bspm:jammy

RUN apt-get update && apt-get install -y --no-install-recommends \
        git \
	jags

RUN git clone https://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@github.com/LandSciTech/Caribou-Demographic-Projection-Paper.git
RUN git clone https://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@github.com/LandSciTech/BayesianCaribouDemographicProjection.git

RUN mkdir R-packages
RUN export R_LIBS=R-packages

RUN mv "/usr/local/lib/R/site-library/bspm" "/usr/lib/R/library"

RUN nohup Rscript --vanilla "Caribou-Demographic-Projection-Paper/make.R"
