# syntax=docker/dockerfile:1

FROM rocker/shiny:4.5.1

LABEL org.opencontainers.image.title="mlmr"
LABEL org.opencontainers.image.description="Shiny app for mixed-effects and multilevel models in R"
LABEL org.opencontainers.image.source="https://github.com/MarcusHarrisUConn/mlmr"
LABEL org.opencontainers.image.url="https://marcusharrisphd.com/mlmr/"
LABEL org.opencontainers.image.licenses="MIT"

ENV PORT=3838

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny \
    bslib \
    lme4 \
    ggplot2 \
    haven \
    knitr \
    readxl \
    rmarkdown \
  && rm -rf /tmp/downloaded_packages

WORKDIR /opt/mlmr

COPY DESCRIPTION NAMESPACE LICENSE ./
COPY R ./R
COPY inst ./inst
COPY man ./man
COPY vignettes ./vignettes
COPY tests ./tests

RUN R CMD INSTALL --no-multiarch --with-keep.source . \
  && useradd --create-home --shell /usr/sbin/nologin mlmr \
  && chown -R mlmr:mlmr /opt/mlmr

USER mlmr
WORKDIR /home/mlmr

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=45s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:${PORT}/" >/dev/null || exit 1

CMD ["R", "-q", "-e", "shiny::runApp(system.file('app', package = 'mlmr'), host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '3838')), launch.browser = FALSE)"]
