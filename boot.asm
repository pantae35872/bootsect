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
PIT_FREQ equ 1000
PIT_FREQ_D = 1193180 / PIT_FREQ

; VGA
SCREEN_WIDTH equ 320
SCREEN_HEIGHT equ 200

; Snake
SNAKE_DIRECTION_UP equ 0
SNAKE_DIRECTION_DOWN equ 1
SNAKE_DIRECTION_LEFT equ 2
SNAKE_DIRECTION_RIGHT equ 3

BOARD_X_SIZE equ 20
BOARD_Y_SIZE equ 20

SNAKE_WIDTH equ 10
SNAKE_HEIGHT equ 10

BOARD_EMPTY equ 0
SNAKE_U equ 1
SNAKE_D equ 2
SNAKE_L equ 3
SNAKE_R equ 4
APPLE equ 5

GAME_SPEED equ 150

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
blkcnt:	dw	3		; int 13 resets this to # of blocks actually read/written
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
    snake_direction db ?
    board rb BOARD_X_SIZE * BOARD_Y_SIZE
    c_board rb BOARD_X_SIZE * BOARD_Y_SIZE
    eaten db ?
    c_eaten db ?
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

  call game_start
.done:
  mov cl, BRIGHT_GREEN
  call clear_screen
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
  
; Draw a rectangle
; inputs:
; ebx = x position
; eax = y position 
; edi = width
; edx = height
; cl = color
; dirty: eax, ebx, edi, edx, esi
; return: none
draw_rect:
  mov esi, ebx
  add edx, eax
  add edi, esi
  .loop_y:
    mov ebx, esi
    .loop_x:
      push edx
      push edi
      call put_pixel
      pop edi
      pop edx
      inc ebx
      cmp ebx, edi
      jne .loop_x
    inc eax
    cmp eax, edx
    jne .loop_y
  ret

; Draw a block with a specified color
; inputs:
; cl = color
; ebx = x position
; eax = y position
draw_block:
  push eax
  push ebx
  push edi
  push edx
  push esi
  mov edi, SNAKE_WIDTH
  mov edx, SNAKE_HEIGHT
  call draw_rect
  pop esi
  pop edx
  pop edi
  pop ebx
  pop eax
  ret

times 510 - ($-$$) db 0
dw 0xaa55

; ----------------------------------
; SECTOR 2 - 3
; ----------------------------------

org 0x7e00

keyboard_isr:
  pushad
  inb 0x60, al
  cmp al, 0xE0
  je .read_extended
  mov byte [latest_key], al
  jmp .snake
.read_extended:
  mov byte [latest_key + 1], al
  inb 0x60, al
  mov byte [latest_key], al
.snake:
  mov ax, [latest_key]
  cmp ax, 0xE048
  je .snake_up
  cmp ax, 0xE050
  je .snake_down
  cmp ax, 0xE04B
  je .snake_left
  cmp ax, 0xE04D
  je .snake_right
  jmp .done
  .snake_left:
    mov al, [snake_direction]
    cmp al, SNAKE_DIRECTION_RIGHT
    je .done
    mov byte [snake_direction], SNAKE_DIRECTION_LEFT
    jmp .done
  .snake_right:
    mov al, [snake_direction]
    cmp al, SNAKE_DIRECTION_LEFT
    je .done
    mov byte [snake_direction], SNAKE_DIRECTION_RIGHT
    jmp .done
  .snake_up:
    mov al, [snake_direction]
    cmp al, SNAKE_DIRECTION_DOWN
    je .done
    mov byte [snake_direction], SNAKE_DIRECTION_UP
    jmp .done
  .snake_down:
    mov al, [snake_direction]
    cmp al, SNAKE_DIRECTION_UP
    je .done
    mov byte [snake_direction], SNAKE_DIRECTION_DOWN
    jmp .done
.done:
  mov eax, 1
  call pic_eoi
  popad
  iret
  
pit_freq:
  mov al, 0x36
  out 0x43, al
  mov al, PIT_FREQ_D and 0xFF
  out 0x40, al
  mov al, PIT_FREQ_D shr 8
  out 0x40, al
  ret

game_start:
.reset_board:
  mov ecx, 1
  imul ecx, BOARD_X_SIZE
  mov ebx, 1
  mov byte [board + ecx + ebx], SNAKE_D
  mov ecx, 2
  imul ecx, BOARD_X_SIZE
  mov ebx, 1
  mov byte [board + ecx + ebx], SNAKE_D
  mov ecx, 3
  imul ecx, BOARD_X_SIZE
  mov ebx, 1
  mov byte [board + ecx + ebx], SNAKE_D
  mov ecx, 10
  imul ecx, BOARD_X_SIZE
  mov ebx, 10
  mov byte [board + ecx + ebx], APPLE
  mov byte [snake_direction], SNAKE_DIRECTION_DOWN
.game_loop:
  mov eax, GAME_SPEED
  call wait_millis ;| wait for 1 second
  call render_board
  call move_snake
  mov ax, [latest_key]
  cmp ax, 0x81
  je .exit
  jmp .game_loop
.exit:
  ret

clone_board:
  xor ecx, ecx
.loop:
  mov al, [board + ecx]
  mov byte [c_board + ecx], al
  inc ecx
  cmp ecx, BOARD_Y_SIZE * BOARD_X_SIZE
  jne .loop
  ret

clone_board_back:
  xor ecx, ecx
.loop:
  mov al, [c_board + ecx]
  mov byte [board + ecx], al
  inc ecx
  cmp ecx, BOARD_Y_SIZE * BOARD_X_SIZE
  jne .loop
  ret

; Generate the head based on the current direction
; inputs:
; ecx = current head Y position
; ebx = current head X position
; al = current head
; dirty: dl
; return: none
generate_snake:
  push ecx
  push ebx
  push ax
  mov dl, byte [snake_direction] 
  inc dl ; convert from 0-3 to 1-4
  cmp al, SNAKE_U
  je .snake_up
  cmp al, SNAKE_D
  je .snake_down
  cmp al, SNAKE_L
  je .snake_left
  cmp al, SNAKE_R
  je .snake_right
.snake_up:
  dec ecx
  jmp .check_apple
.snake_down:
  inc ecx
  jmp .check_apple
.snake_right:
  inc ebx
  jmp .check_apple
.snake_left:
  dec ebx
.check_apple:
  imul ecx, BOARD_X_SIZE

  mov al, byte [c_board + ecx + ebx]
  cmp al, APPLE
  je .apple
  jmp .done
.apple:
  mov byte [c_eaten], 1
.done:
  mov byte [c_board + ecx + ebx], dl
  pop ax
  pop ebx
  pop ecx
  ret

move_snake:
  call clone_board
  mov al, [eaten]
  mov byte [c_eaten], al
  xor ecx, ecx ; ecx is the y position
.loop_y:   
  xor ebx, ebx ; ebx is the x position
  .loop_x:
    push ecx
    imul ecx, BOARD_X_SIZE
    mov al, [board + ecx + ebx]
    pop ecx
    call check_snake
    cmp dl, 1
    je .remove_snake
    cmp dl, 2 
    je .generate_snake
    jmp .nothing
    .remove_snake:
      mov ah, byte [eaten]
      cmp ah, 1
      je .reset_apple

      push ecx
      imul ecx, BOARD_X_SIZE
      mov byte [c_board + ecx + ebx], BOARD_EMPTY
      pop ecx
      jmp .nothing
    .generate_snake:
      call generate_snake
      jmp .nothing
    .reset_apple:
      mov byte [c_eaten], 0
      call generate_apple
    .nothing:
    inc ebx
    cmp ebx, BOARD_X_SIZE
    jne .loop_x
  inc ecx
  cmp ecx, BOARD_Y_SIZE
  jne .loop_y
.done:
  call clone_board_back
  mov al, byte [c_eaten]
  mov byte [eaten], al
  ret

; Generate a random apple
; inputs: none
; dirty: none
; return: none
generate_apple:
  push eax
  push ebx
  push ecx
.generate_apple:
  call randx
  mov ebx, eax
  call randy
  mov ecx, eax
  imul ecx, BOARD_X_SIZE
  mov al, byte [board + ecx + ebx]
  call is_snake
  cmp bl, 1
  je .generate_apple
  mov byte [c_board + ecx + ebx], APPLE
.done:
  pop ecx
  pop ebx
  pop eax
  ret

; Generate a random number for the x coordinate
; inputs: none
; dirty: none
; return: eax = random number between 0-BOARD_X_SIZE
randx:
  push edx
  rdtsc                       ; EDX:EAX = timestamp counter
  mov ebx, BOARD_X_SIZE - 1   ; divisor for modulo
  xor edx, edx                ; clear EDX before division (required for div)
  div ebx                     ; EAX = EAX / 20, EDX = EAX % 20
  mov eax, edx                ; EAX = random number between 0–19
  pop edx
  ret
; Generate a random number for the y coordinate
; inputs: none
; dirty: none
; return: eax = random number between 0-BOARD_Y_SIZE
randy:
  push edx
  rdtsc                       ; EDX:EAX = timestamp counter
  mov ebx, BOARD_Y_SIZE - 1   ; divisor for modulo
  xor edx, edx                ; clear EDX before division (required for div)
  div ebx                     ; EAX = EAX / 20, EDX = EAX % 20
  mov eax, edx                ; EAX = random number between 0–19
  pop edx
  ret

; Chck if snake is a head or a tail or body
; inputs:
; al = Snake
; ebx = X position
; ecx = Y position
; dirty: none
; return: 
; dl = 3 if it's a body, 1 if it's a tail, 2 if it's a head, 0 if it's not a snake
check_snake:
  push edi
  push ebx
  push eax

  xor dl, dl

  push bx
  call is_snake ; Check If theres a snake right
  cmp bl, 0
  pop bx
  je .done

  push ax
  push bx
  mov edi, ebx
  call get_snake_neighbours
  cmp al, SNAKE_R
  je .got_pointed
  cmp ah, SNAKE_L
  je .got_pointed
  cmp bl, SNAKE_D
  je .got_pointed
  cmp bh, SNAKE_U
  je .got_pointed
  jmp .next
.got_pointed:
  or dl, 10b 
.next:
  pop bx
  pop ax

  cmp al, SNAKE_U
  je .snake_up
  cmp al, SNAKE_D
  je .snake_down
  cmp al, SNAKE_L
  je .snake_left
  cmp al, SNAKE_R
  je .snake_right
  jmp .done

.snake_up:
  mov edi, ebx
  call get_snake_neighbours

  mov al, bl
  call is_snake ; If theres a snake upwards
  or dl, bl

  jmp .done

.snake_down:
  mov edi, ebx
  call get_snake_neighbours

  mov al, bh
  call is_snake ; If theres a snake downwards
  or dl, bl
  jmp .done
.snake_right:
  mov edi, ebx
  call get_snake_neighbours

  mov al, ah
  call is_snake ; If theres a snake right
  or dl, bl

  jmp .done
.snake_left:
  mov edi, ebx
  call get_snake_neighbours

  call is_snake ; If theres a snake on the left
  or dl, bl

.done:
  pop eax
  pop ebx
  pop edi
  ret

; Get the snake neighbours
; inputs:
; edi = X position
; ecx = Y position
; dirty: none
; return: 
; al = left neightbour
; ah = right neighbour
; bl = up neighbour
; bh = down neighbour
get_snake_neighbours:
  push edx
  push edi
  push ecx
  ; Save ecx to edx for later use
  mov edx, ecx

  ; Down neighbour
  inc ecx
  imul ecx, BOARD_X_SIZE
  mov bh, [board + ecx + edi]
  ; Up neighbour
  mov ecx, edx
  dec ecx
  imul ecx, BOARD_X_SIZE
  mov bl, [board + ecx + edi]

  mov ecx, edx
  imul ecx, BOARD_X_SIZE

  ; Right neighbour
  mov edx, edi
  inc edi
  mov ah, [board + ecx + edi]

  ; Left neighbour
  mov edi, edx
  dec edi
  mov al, [board + ecx + edi]

  pop ecx
  pop edi
  pop edx
  ret

; Check if the byte is a snake
; inputs:
; al = byte
; dirty: none
; return: bl = 1 if it is a snake, 0 if not
is_snake:
  cmp al, SNAKE_U
  je .is_snake
  cmp al, SNAKE_D
  je .is_snake
  cmp al, SNAKE_L
  je .is_snake
  cmp al, SNAKE_R
  je .is_snake
  xor bl, bl
  jmp .done
.is_snake:
  mov bl, 1
.done:
  ret

render_board:
  xor ecx, ecx ; ecx is the y position
  .loop_y:
    xor ebx, ebx ; ebx is the x position
    .loop_x:
      push ecx
      imul ecx, BOARD_X_SIZE
      mov al, [board + ecx + ebx]
      pop ecx
      mov esi, ecx
      cmp al, BOARD_EMPTY
      je .draw_empty
      push bx
      call is_snake
      cmp bl, 1
      pop bx
      je .draw_snake
      cmp al, APPLE
      je .draw_apple
      .draw_empty:
        mov cl, BLUE
        jmp .draw
      .draw_snake:
        mov cl, BRIGHT_GREEN
        jmp .draw
     .draw_apple:
        mov cl, BRIGHT_RED
      .draw:
        push ebx
        imul ebx, SNAKE_WIDTH
        mov eax, esi
        imul eax, SNAKE_HEIGHT
        call draw_block
        pop ebx
      mov ecx, esi
      inc ebx
      cmp ebx, BOARD_X_SIZE
      jne .loop_x
    inc ecx
    cmp ecx, BOARD_Y_SIZE
    jne .loop_y
.done:
  ret


; Wait in milliseconds (Spin the processor until the completely useless time has passed)
; inputs: 
; eax = milliseconds
; dirty: none
; return: none
wait_millis:
  push eax
  push ecx
  push ebx
  xor ecx, ecx
  mov ebx, eax 
  imul ebx, PIT_FREQ / 1000
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
; cl = color
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

times 1536 - ($-$$) db 0
