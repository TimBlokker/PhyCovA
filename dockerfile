#include the treetime python module and its dependencies
#FROM python:3.8-slim-buster AS treetime_image
#RUN apt-get update
#RUN python -m venv /opt/PhyCovA
#ENV PATH="/opt/PhyCovA/bin:$PATH"
#COPY treetime-master treetime-master/.
#RUN pip install treetime-master/.


# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
#artifacts from previous stage
#COPY --from=treetime_image /opt/PhyCovA /opt/PhyCovA
# renv.lock file
COPY renv.lock ./renv.lock
## app folder
COPY . ./app

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'


#docker hub########################################################################
#EXPOSE 3838
#RUN chmod +x app/changeSymlink.sh
#RUN app/changeSymlink.sh
#ENV PATH="/opt/PhyCovA/bin:$PATH"
#CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
#\dockerhub########################################################################

####horeku#########################################################################################               
RUN rm -rf /var/lib/apt/lists/*              
RUN chmod +x app/changeSymlink.sh
#RUN app/changeSymlink.sh
ENV PATH="/opt/PhyCovA/bin:$PATH"
RUN useradd shiny_user
USER shiny_user
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]
####\horeku########################################################################################                 
