#!/bin/bash

# Small helper to set DISPLAY and start services under the current user if needed.
export DISPLAY=:99

# Ensure noVNC websockify path exists
if [ ! -d /opt/noVNC ]; then
  echo "noVNC not found at /opt/noVNC"
  exit 1
fi

# Start supervisord
exec /usr/bin/supervisord -n -c /etc/supervisor/supervisord.conf
