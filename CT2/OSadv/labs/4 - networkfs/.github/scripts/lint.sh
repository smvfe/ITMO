#!/bin/bash

# Lints source tree and formats errors according to GitHub Actions Commands.
#
# clang-format and rustfmt are available, all other tools can be installed
# using apt or any other way of your choice.
#
# Should be run from source root.

set -euo pipefail
shopt -s globstar

clang-format --dry-run -Werror **/*.c **/*.h 2>clang-format.log || status=$? && status=$?
if [[ -n $status ]]; then
  cat clang-format.log |
    grep 'error:' |
    perl -pe 's/^((.*?):(.*?):(.*?):.*)$/::error file=\2,line=\3,col=\4::\1/g' || true
  exit $status
fi

exit 0