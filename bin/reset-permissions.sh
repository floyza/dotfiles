#!/usr/bin/env zsh
set -euo pipefail

chown -R "$USER":"$(id -g -n)" "$1"

find "$1" -type f -exec chmod $((0666 - $(umask))) {} \;
find "$1" -type d -exec chmod $((0777 - $(umask))) {} \;
