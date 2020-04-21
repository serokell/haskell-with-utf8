#!/usr/bin/env bash

stack exec utf8-troubleshoot 2>/dev/null
printf '\n--\n\n'
$(stack exec -- bash -c 'command -v utf8-troubleshoot' 2>/dev/null)
