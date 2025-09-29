FROM rocker/r-ver:4.3.2

# Consolidated image: R (pinned), Python, Jupyter, and noVNC stack for an in-container browser
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies for R, building packages, headful browser, and noVNC stack
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       python3 \
       python3-pip \
       git \
       wget \
       xvfb \
       x11vnc \
       fluxbox \
       supervisor \
       dbus-x11 \
       ca-certificates \
       chromium-browser \
    && rm -rf /var/lib/apt/lists/*

# Install JupyterLab and Python requirements
COPY requirements.txt /tmp/requirements.txt
RUN python3 -m pip install --no-cache-dir jupyter jupyterlab jupyter-client websockify \
    && python3 -m pip install --no-cache-dir -r /tmp/requirements.txt \
    && rm -rf /tmp/requirements.txt /root/.cache /tmp/*

# Install noVNC
RUN git clone --depth 1 https://github.com/novnc/noVNC /opt/noVNC \
    && git clone --depth 1 https://github.com/novnc/websockify /opt/noVNC/utils/websockify

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
    && chmod 0440 /etc/sudoers.d/$USERNAME \
    && mkdir -p /var/log/supervisor \
    && chown -R $USERNAME:$USERNAME /var/log/supervisor \
    && rm -rf /var/lib/apt/lists/*

# Add supervisor config and a start script for the noVNC/browser stack
COPY .devcontainer/supervisord-novnc.conf /etc/supervisor/conf.d/supervisord-novnc.conf
COPY .devcontainer/start-novnc.sh /usr/local/bin/start-novnc.sh
RUN chmod +x /usr/local/bin/start-novnc.sh

USER $USERNAME
WORKDIR /home/$USERNAME/workspace

EXPOSE 8888 6080

# Start supervisor as PID 1 so it manages the browser/VNC services; keep bash available via exec
ENTRYPOINT ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisor/supervisord.conf"]