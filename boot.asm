use16
org 0x7c00
jmp start

KEYBOARD_SERVICES equ 0x16
VIDEO_SERVICES equ 0x10

; Colors
BLACK equ 0x00
BLUE equ 0x01
GREEN equ 0x02
CYAN equ 0x03
RED equ 0x04
MAGENTA equ 0x05
BROWN equ 0x06
WHITE equ 0x07
DARK_GRAY equ 0x08
BRIGHT_BLUE equ 0x09
BRIGHT_GREEN equ 0x0A
BRIGHT_CYAN equ 0x0B
BRIGHT_RED equ 0x0C
BRIGHT_MAGENTA equ 0x0D
BRIGHT_YELLOW equ 0x0E
BRIGHT_WHITE equ 0x0F

; GDT Segments
CODE_SEG equ 0x08
DATA_SEG equ 0x10

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

macro interrupt_gate offset, is_trap
{
  dw offset and 0xFFFF
  dw CODE_SEG
  db 0x00
  if is_trap
    db 10001111b
  else
    db 10001110b
  end if
  dw offset shr 16
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

IDT:
interrupt_gate hlt_loop, 1 ; Divide Error
interrupt_gate hlt_loop, 1 ; Debug Exception
interrupt_gate hlt_loop, 0 ; NMI Interrupt
interrupt_gate hlt_loop, 1 ; Breakpoint Exception
interrupt_gate hlt_loop, 1 ; Overflow Exception
interrupt_gate hlt_loop, 1 ; BOUND Range Exceeded Exception
interrupt_gate hlt_loop, 1 ; Invalid Opcode Exception
interrupt_gate hlt_loop, 1 ; Device Not Available Exception
interrupt_gate hlt_loop, 1 ; Double Fault Exception
interrupt_gate hlt_loop, 1 ; Coprocessor Segment Overrun
interrupt_gate hlt_loop, 1 ; Invalid TSS
interrupt_gate hlt_loop, 1 ; Segment Not Present
interrupt_gate hlt_loop, 1 ; Stack Fault
interrupt_gate hlt_loop, 1 ; General Protection Exception
interrupt_gate hlt_loop, 1 ; Page Fault
interrupt_gate hlt_loop, 1 ; Reserved
interrupt_gate hlt_loop, 1 ; Floating Point Error
interrupt_gate hlt_loop, 1 ; Alignment Check
interrupt_gate hlt_loop, 1 ; Machine Check
interrupt_gate hlt_loop, 1 ; SIMD Floating Point Exception
interrupt_gate hlt_loop, 1 ; Virtualization Exception
interrupt_gate hlt_loop, 1 ; Control Protection Exception
repeat 10
  interrupt_gate hlt_loop, 1 ; Reserved
end repeat
IDT_END:

IDT_PTR:
  dw IDT_END-IDT-1
  dd IDT

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

hlt_loop:
  hlt
  jmp hlt_loop

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
  mov cl, BRIGHT_GREEN
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
