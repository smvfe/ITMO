#!/bin/bash

# Runs tests in virtual machine.
#
# Usage: ./run_vm.sh disk.img gtest_args [--debug]
#
#   disk.img   -- path to disk image mounted to /place
#   gtest_args -- argument to pass to gtest, e.g. --gtest_filter=BaseTest.*
#   --debug    -- optional flag to view entire dmesg in logs

disk_img=$1
gtest_args=$2

if [[ "$3" == "--debug" ]]; then
    gtest_debug="gtest_debug"
else
    gtest_debug=""
fi

qemu-system-x86_64 \
    -kernel /opt/vmlinuz-6.8.0-45-generic \
    -drive file=/opt/ubuntu.img,format=raw,index=0,media=disk \
    -drive file=$1,format=raw,index=1,media=disk \
    -append "apparmor=0 root=/dev/sda console=ttyS0 gtest_args=\"$gtest_args\" $gtest_debug" \
    -nographic \
    -serial mon:stdio \
    -smp 2 \
    -m 512M |
    tee qemu.log

fgrep 'networkfs_test exited with code 0' qemu.log > /dev/null
