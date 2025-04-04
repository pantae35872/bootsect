#!/bin/bash

set -e

fasm boot.asm

qemu-system-x86_64 --drive format=raw,file=boot.bin
