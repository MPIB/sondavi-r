#!/usr/bin/env bash
# The whole R test run in one command: start the fixture server, run the checks,
# stop it again.
#
#   tests/run.sh
set -uo pipefail
cd "$(dirname "$0")/.."

python3 tests/fixture-server.py 8765 &
SERVER=$!
trap 'kill $SERVER 2>/dev/null' EXIT
sleep 1

Rscript tests/run-tests.R
