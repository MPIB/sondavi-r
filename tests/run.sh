#!/usr/bin/env bash
# Bequemlichkeit fuer die Kommandozeile; die Arbeit macht tests/run.py, damit beide
# Klientenpakete denselben Pruefstand-Start benutzen.
exec "$(command -v python3 || command -v python)" "$(dirname "$0")/run.py"
