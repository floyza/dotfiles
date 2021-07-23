#!/usr/bin/env bash
set -euo pipefail

rm -rf /etc/nixos
mkdir /etc/nixos
rsync -a "$(dirname "$0")" /etc/nixos \
    --exclude=.git --exclude="$(basename "$0")"
