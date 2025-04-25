FROM docker.io/jupyter/datascience-notebook:latest

USER root
# Generally, Dev Container Features assume that the non-root user (in this case jovyan)
# is in a group with the same name (in this case jovyan). So we must first make that so.
RUN groupadd jovyan \
  && usermod -g jovyan -a -G users jovyan

USER jovyan

RUN curl -L https://dot.net/v1/dotnet-install.sh -o dotnet-install.sh \
    && ./dotnet-install.sh \

ENV PATH="$PATH:~/.dotnet/"

RUN dotnet tool install fable \
    && dotnet tool install paket
