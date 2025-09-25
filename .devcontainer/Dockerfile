FROM rocker/r-ver:4.3.2

# Install system dependencies needed for many R packages and for Python/Jupyter
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       python3 \
       python3-pip \
       git \
    && rm -rf /var/lib/apt/lists/*

# Install JupyterLab and Python requirements
COPY ../requirements.txt /tmp/requirements.txt
RUN python3 -m pip install --no-cache-dir -r /tmp/requirements.txt jupyterlab

# Install IRkernel and register system-wide
RUN R -q -e "install.packages(c('jsonlite','languageserver','IRkernel'), repos='https://cloud.r-project.org')" \
    && R -q -e "IRkernel::installspec(user = FALSE)"

# Install renv so projects can pin R package versions (restore will run at container start if renv.lock is present)
RUN R -q -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Create a non-root user to match Codespaces default user behavior
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && apt-get update && apt-get install -y --no-install-recommends sudo \
    && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

USER $USERNAME
WORKDIR /home/$USERNAME/workspace

EXPOSE 8888

CMD ["/bin/bash"]
# Universal image includes Python, Node, Conda, and Jupyter
FROM mcr.microsoft.com/devcontainers/universal:2-linux

# Install R from Ubuntu repos and IRkernel system-wide
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base r-base-dev libcurl4-openssl-dev libssl-dev libxml2-dev \
 && rm -rf /var/lib/apt/lists/*

# Install IRkernel and register kernelspec for all users
RUN R -q -e "install.packages(c('languageserver','jsonlite','IRkernel'), repos='https://cloud.r-project.org')"