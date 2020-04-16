FROM rocker/rstudio

RUN sudo apt-get update -y \
&& sudo apt -y install default-jdk \
&& apt-get -y install r-cran-rjava 

