#!/bin/bash

# Builds kernel module.
#
# Should be run from source root.

set -euo pipefail

mkdir -p build
cd build
cmake -DKERNELHEADERS_DIR=/usr/src/linux-headers-6.8.0-45-generic ..
make networkfs networkfs_test
