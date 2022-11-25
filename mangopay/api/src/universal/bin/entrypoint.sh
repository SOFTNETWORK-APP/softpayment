#!/usr/bin/env bash
set -e
set -u
set -o pipefail

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

function printError() {
    echo -e "${RED}$1${NC}" 1>&2
}

function printInfo() {
    echo -e "${GREEN}$1${NC}" 1>&2
}

function clean() {
    IFS=$SAVEIFS
    exit $1
}

export CLUSTER_IP=$(hostname -i | awk '{print $1}')

/opt/docker/bin/mangopay-api -DCLUSTER_MANAGEMENT_HOST=$CLUSTER_IP
