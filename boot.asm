use16
org 0x7c00
jmp start

; BIOS SERVICES
KEYBOARD_SERVICES equ 0x16
VIDEO_SERVICES equ 0x10
DISK_SERVICES equ 0x13

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

; PIC
PIC1 equ 0x20
PIC2 equ 0xA0
PIC1_CMD equ PIC1
PIC1_DATA = PIC1 + 1
PIC2_CMD equ PIC2
PIC2_DATA = PIC2 + 1

PIC1_OFFSET equ 0x20
PIC2_OFFSET equ 0x28

PIC_EOI equ 0x20
ICW1_ICW4	equ 0x01		    ; Indicates that ICW4 will be present 
ICW1_SINGLE	equ 0x02		  ; Single (cascade) mode
ICW1_INTERVAL4 equ 0x04		; Call address interval 4 (8)
ICW1_LEVEL equ 0x08		    ; Level triggered (edge) mode
ICW1_INIT equ 0x10		    ; Initialization - required!

ICW4_8086	equ 0x01		    ; 8086/88 (MCS-80/85) mode
ICW4_AUTO	equ 0x02		    ; Auto (normal) EOI
ICW4_BUF_SLAVE equ 0x08		; Buffered mode/slave
ICW4_BUF_MASTER	equ 0x0C	; Buffered mode/master
ICW4_SFNM	equ 0x10		    ; Special fully nested (not)

; PIT
PIT_FREQ equ 100
PIT_FREQ_D = 1193180 / PIT_FREQ

; VGA
SCREEN_WIDTH equ 320
SCREEN_HEIGHT equ 200

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

macro outb port, value
{
  mov dx, port
  mov al, value
  out dx, al
}

macro inb port, reg 
{
  mov dx, port
  in reg, dx
}

macro io_wait
{
  outb 0x80, 0
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
interrupt_gate break_point_isr, 1 ; Breakpoint Exception
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
interrupt_gate timer_isr, 0 ; System Timer
interrupt_gate keyboard_isr, 0 ; System Timer
IDT_END:

IDT_PTR:
  dw IDT_END-IDT-1
  dd IDT

DAPACK:
	db	0x10
	db	0
blkcnt:	dw	1		; int 13 resets this to # of blocks actually read/written
db_add:	dw	0x7E00		; memory buffer destination address (0:7c00)
	dw	0		; in memory page zero
d_lba:	dd	1		; put the lba to read in this spot
	dd	0		; more storage bytes only for big lba's ( > 4 bytes )

; ----------------------------------
; RUNTIME VARIABLES
; ----------------------------------

virtual at 0x500
    tick_count dd ?
    latest_key dw ?
    ;my_struct:
    ;    .field1 dd ?
    ;    .field2 db ?
end virtual

start:
  ; Setup segments
  xor ax, ax
  mov ds,ax
  mov es,ax
  mov ax, 0x9000 ; stack 0x9000-0xFFFF
  mov ss,ax
  mov sp, 0xFFFF
  
  mov al, 0x13
  bios_int VIDEO_SERVICES, 0 
  
  ; Load the second sector
	mov si, DAPACK		; address of "disk address packet"
	mov dl, 0x80		; drive number 0 (OR the drive # with 0x80)
  biosx_int DISK_SERVICES, 0x4200 ; AX unused

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

protected_mode_entry:
	; Reload stack and data segment registers with GDT entry
	mov ax, DATA_SEG
	mov ds, ax
	mov ss, ax
	mov sp, 0xffff

  ; Load IDT
  lidt [IDT_PTR]

  call pic_remap
  call pit_freq
  sti
  
  mov cl, BRIGHT_GREEN
.loop:
  mov ax, [latest_key]
  cmp ax, 0x90
  je .done
  mov eax, 1
  call wait_seconds
  call clear_screen
  inc cl
  jmp .loop
.done:
  cli
  hlt

timer_isr:
  pushad
  inc dword [tick_count]
  mov eax, 0
  call pic_eoi
  popad
  iret

break_point_isr:
  pushad
  popad
  iret

times 510 - ($-$$) db 0
dw 0xaa55


; ----------------------------------
; SECTOR 2
; ----------------------------------

org 0x7e00

keyboard_isr:
  pushad
  inb 0x60, al
  cmp al, 0xE0
  je .read_extended
  mov byte [latest_key], al
  jmp .done
.read_extended:
  mov byte [latest_key + 1], al
  inb 0x60, al
  mov byte [latest_key], al
.done:
  mov eax, 1
  call pic_eoi
  popad
  iret

; Wait in seconds (Spin the processor until the completely useless time has passed)
; inputs: 
; eax = seconds
; dirty: none
; return: none
wait_seconds:
  push eax
  push ecx
  push ebx
  xor ecx, ecx
  mov ebx, eax 
  imul ebx, PIT_FREQ
  mov eax, [tick_count]
  add ebx, eax
.loop:
  mov eax, [tick_count]
  cmp eax, ebx
  jae .done
  hlt 
  jmp .loop
.done:
  pop ebx
  pop ecx
  pop eax
  ret  

pit_freq:
  mov al, 0x36
  out 0x43, al
  mov al, PIT_FREQ_D and 0xFF
  out 0x40, al
  mov al, PIT_FREQ_D shr 8
  out 0x40, al
  ret

; Remaps the PIC
; inputs: none
; dirty: dx, al
; return: none
pic_remap:
  outb PIC1_CMD, ICW1_INIT or ICW1_ICW4
  io_wait
  outb PIC2_CMD, ICW1_INIT or ICW1_ICW4
  io_wait
  outb PIC1_DATA, PIC1_OFFSET
  io_wait
  outb PIC2_DATA, PIC2_OFFSET 
  io_wait
  outb PIC1_DATA, 4 ; tell pic1 that pic2 is at IRQ2
  io_wait
  outb PIC2_DATA, 2 ; tell pic2 its cascade identity
  io_wait

  outb PIC1_DATA, ICW4_8086
  io_wait
  outb PIC2_DATA, ICW4_8086
  io_wait

  ; Unmask all interrupts
  outb PIC1_DATA, 0x00
  outb PIC2_DATA, 0x00
  ret

; Sends eoi to pic controller
; inputs:
; eax = interrupt number
; dirty: ecx
; return: none
pic_eoi:
  push dx 
  push bx
  mov ecx, eax ; Save interrupt number
  mov bl, PIC_EOI
  outb PIC1_CMD, bl
  cmp ecx, 8 ; if interrupt is from pic2
  jae .pic2  ; also send eoi to pic2
  jmp .done
.pic2:
  outb PIC2_CMD, bl
.done:
  pop bx
  pop dx
  mov eax, ecx
  ret

; Put pixel on the framebuffer
; inputs:
; ebx = x posisiton
; eax = y posision
; dl = color
; dirty: edi, edx
; return: none
put_pixel:
  mov edx, eax ; saves eax into eax
  mov edi, 0xA0000            ;|
  imul eax, SCREEN_WIDTH      ;| 
  add edi, eax                ;|-- edi = 0xA0000 + 320 * eax + ebx
  add edi, ebx                ;|
  mov byte [edi], cl
  mov eax, edx ; restore eax
  ret

; Clear a screen to a specifed color
; inputs:
; cl = color
; dirty: eax, ebx, edi, edx
; return: none
clear_screen:
  xor eax, eax              
.loop:                       
  xor ebx, ebx              
  .loop1:                   
    call put_pixel         
    inc ebx                 
    cmp ebx, SCREEN_WIDTH   
    jne .loop1              
  inc eax                   
  cmp eax, SCREEN_HEIGHT    
  jne .loop                 
.done:
  ret

times 512 - ($-$$) db 0


