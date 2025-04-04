use16
org 0x7c00
jmp start

KEYBOARD_SERVICES equ 0x16
VIDEO_SERVICES equ 0x10

macro bios_int service,code
{
  mov ah, code
  int service
}

macro biosx_int service,code
{
  mov ax, code
  int service
}


GDT:
  dd 0
  dd 0

  dw 0xFFFF
  dw 0
  db 0
  db 10011010b
  db 11001111b
  db 0

  dw 0xFFFF
  dw 0
  db 0
  db 10010010b
  db 11001111b
  db 0
GDT_END:

GDT_PTR:
  dw GDT_END-GDT-1
  dd GDT

CODE_SEG equ 0x08
DATA_SEG equ 0x10

start:
  ; Setup segments
  xor ax, ax
  mov ds,ax
  mov es,ax
  mov ax, 0x9000 ; stack 0x9000-0xFFFF
  mov ss,ax
  mov sp, 0xFFFF
  
  ;mov al, 0x13
  mov al, 0x13
  bios_int VIDEO_SERVICES, 0 

   ; Load GDT
  lgdt [GDT_PTR]

  ; Enable protected mode
  mov eax, cr0
  or eax, 1
  mov cr0, eax

  cli
  jmp CODE_SEG:protected_mode_entry

use32

; Put pixel on the framebuffer
; inputs:
; ebx = x posisiton
; eax = y posision
; dl = color
; dirty: edi, edx
; return: none
put_pixel:
  mov edx, eax ; saves eax into eax
  mov edi, 0xA0000  ;|
  imul eax, 320     ;| 
  add edi, eax      ;|-- edi = 0xA0000 + 320 * eax + ebx
  add edi, ebx      ;|
  mov byte [edi], cl
  mov eax, edx ; restore eax
  ret

protected_mode_entry:
	; Reload stack and data segment registers with GDT entry
	mov ax, DATA_SEG
	mov ds, ax
	mov ss, ax
	mov sp, 0xffff

	;mov si, test_msg	; Setting PM welcome message to be printed
	;call print		; Printing PM Welcome message

  xor eax, eax
  mov cl, 0x0C
.loop:
  xor ebx, ebx
  .loop1:
    call put_pixel
    inc ebx
    cmp ebx, 320
    jne .loop1
  inc eax
  cmp eax, 200
  jne .loop
.done:
  cli
  hlt

times 510 - ($-$$) db 0
dw 0xaa55
