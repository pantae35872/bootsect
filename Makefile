.PHONY: run

all: boot.bin

boot.bin: boot.asm
	fasm boot.asm

run:
	qemu-system-x86_64 -enable-kvm --drive format=raw,file=boot.bin
