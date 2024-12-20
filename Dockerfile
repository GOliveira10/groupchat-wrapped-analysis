FROM rocker/r-ver:4.2.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

RUN R -e "install.packages(c('plumber', 'tidyverse', 'ggplot2', 'lubridate', 'textclean', 'httr'), repos='https://cloud.r-project.org')"

COPY plumber.R /app/plumber.R

EXPOSE 8000
CMD [\"R\", \"-e\", \"pr <- plumber::pr('/app/plumber.R'); pr$run(host='0.0.0.0', port=8000)\"] 
