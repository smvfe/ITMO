#!/bin/bash

# Builds a small disk image containing kernel module and executable with tests.
#
# Should be run from source root.

set -euo pipefail

KERNEL=6.8.0-45-generic
DISK=$(mktemp -d)

cp build/networkfs.ko $DISK/
cp build/networkfs_test $DISK/

export SUPERMIN_KERNEL_VERSION=$KERNEL
export SUPERMIN_KERNEL=/opt/vmlinuz-$KERNEL
export SUPERMIN_MODULES=/opt/kernel-modules/lib/modules/$KERNEL
virt-make-fs --format=raw --type=ext4 $DISK networkfs.img --size=32M

rm -rf $DISK
