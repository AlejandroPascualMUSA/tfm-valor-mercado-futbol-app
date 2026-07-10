FROM rocker/shiny-verse:4.4.2

USER root

RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . /app

RUN Rscript -e "source('install_packages.R')"

RUN python3 -m venv /opt/tfm-rag && \
    /opt/tfm-rag/bin/pip install --upgrade pip && \
    /opt/tfm-rag/bin/pip install -r rag/requirements_minimal.txt

ENV TFM_PYTHON=/opt/tfm-rag/bin/python
ENV PYTHONIOENCODING=utf-8
ENV PYTHONDONTWRITEBYTECODE=1
ENV TFM_DATA_MODE=demo
ENV PATH="/opt/tfm-rag/bin:${PATH}"

RUN mkdir -p /app/reports/generated && \
    chmod -R a+rX /app && \
    chmod -R a+rwX /app/reports /tmp

EXPOSE 3838

CMD ["R", "-e", "options(shiny.host='0.0.0.0'); shiny::runApp('/app', port=as.numeric(Sys.getenv('PORT', 3838)))"]