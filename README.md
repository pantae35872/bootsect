# BootSect

![Project Status](https://img.shields.io/badge/status-not%20finished-orange)
![Build](https://img.shields.io/badge/build-passing-brightgreen)

A snake game that's suppose to fit in a 512 bytes (boot sector)

EDIT:    
BAD NEWS guys it's looks like we need more than 1 sector, because the IDT tables taking up all the spaces.
i mean, i could try to dynamically generated the table at runtime. but nahh, loading another sector seems more fun.

## Build Dependices
* ```fasm``` (https://flatassembler.net/)
* ```qemu``` (optional: require if you want to run it in qemu)
* ```make```

## Getting the source code
```bash
git clone https://github.com/pantae35872/bootsect
cd bootsect
```

## Build and Run
```bash
make # build the thing

make run # run with qemu
```
