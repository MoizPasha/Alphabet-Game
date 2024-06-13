[org 0x0100]
jmp start

oldisr: dd 0
oldisr_timer: dd 0
location_box: dw 3920;middleoflastrow

attribute_alphabets:TIMES 7 dw 0x0120
location_alphabets:dw 3900,3100,3200,3300,3604,3700,3800
speed_alphabets:TIMES 7 dw 1

score_string: db 'Score: '
score:dw 0
fail_string:db 'Miss Count: '
fail:dw 0
fail_update:dw 1
score_update:dw 0
gameover_string :db 'GameOver! The Score is: '
gameover:dw 0


RANDGEN:   
push bp
mov bp,sp      ; generate a rand no using the system time
push ax
push cx
RANDSTART:

        rdtsc
   xor  dx, dx
   mov  cx,[bp+4]
   div  cx         
pop cx
pop ax
pop bp
RET 2

;----------
printnum:               push bp
                                mov bp, sp
                                push es
                                push ax
                                push bx
                                push cx
                                push dx
                                push di

                                mov ax, 0xb800
                                mov es, ax                      ; point es to video base

                                mov ax, [bp+4]          ; load number in ax= 4529
                                mov bx, 10                      ; use base 10 for division
                                mov cx, 0                       ; initialize count of digits

nextdigit:              mov dx, 0                       ; zero upper half of dividend
                                div bx                          ; divide by 10 AX/BX --> Quotient --> AX, Remainder --> DX .....
                                add dl, 0x30            ; convert digit into ascii value
                                push dx                         ; save ascii value on stack

                                inc cx                          ; increment count of values
                                cmp ax, 0                       ; is the quotient zero
                                jnz nextdigit           ; if no divide it again


                                mov di, [bp+6]                  ; point di to top left column
nextpos:                pop dx                          ; remove a digit from the stack
                                mov dh, 0x07            ; use normal attribute
                                mov [es:di], dx         ; print char on screen
                                add di, 2                       ; move to next screen location
                                loop nextpos            ; repeat for all digits on stack

                                pop di
                                pop dx
                                pop cx
                                pop bx
                                pop ax
                                pop es
                                pop bp
                                ret 4
;--------------
clrscr:         push es
                        push ax
                        push di

                        mov ax, 0xb800
                        mov es, ax                                      ; point es to video base
                        mov di, 0                                       ; point di to top left column

nextloc:        mov word [es:di], 0x0720        ; clear next char on screen
                        add di, 2                                       ; move to next screen location
                        cmp di, 4000                            ; has the whole screen cleared
                        jne nextloc                                     ; if no clear next position

                        pop di
                        pop ax
                        pop es
                        ret
;--------------
kbisr:
        pusha

        mov ax,0xb800
        mov es,ax


        in al, 0x60 ; read a char from keyboard port

        cmp al, 0x4B ; l
        jz moveleft ; leave interrupt routine

        cmp al, 0x4D ; r
        jz moveright ; leave interrupt routine


nomatch:
        popa
        jmp far [cs:oldisr] ; call the original ISR

moveleft:
mov di,[location_box]
mov word[es:di],0x0720;removebox
sub di,2

cmp di,3840
jle reset_to_right

jmp redraw_box

moveright:
mov di,[location_box]
mov word[es:di],0x0720;removebox
add di,2

cmp di,4000
jge reset_to_left

redraw_box:
mov [location_box],di
cmp byte[es:di],0x20
je got_no_alpha

mov word[score_update],1
inc word[score]
call printscore

got_no_alpha:
mov word[es:di],0x07DC
jmp exit_kbsir


exit_kbsir:
        mov al, 0x20
        out 0x20, al ; end of interrupt
        popa
        iret ; return from interruptkbisr:

reset_to_right:
mov di,3998
jmp redraw_box
reset_to_left:
mov di,3842
jmp redraw_box
;------------


movedown:
push bp
mov bp,sp
pusha

mov ax,0xb800
mov es,ax

mov si,[bp+4]
loop_movedown:
mov di,[location_alphabets+si]
mov ax,[attribute_alphabets+si]
cmp di,[location_box]
je skip_remove_alpha
mov word[es:di],0x720
skip_remove_alpha:
add di,160
cmp di,4000
jl skip_generate_new_alphabet

new_alpha:
cmp word[score_update],0
jz normal_new_alpha
dec word[fail]
mov word[score_update],0

normal_new_alpha:
inc word[fail]
mov word[fail_update],1
push si
call generate_alphabet
mov di,[location_alphabets+si]
jmp redraw_alpha

skip_generate_new_alphabet:
mov [location_alphabets+si],di

redraw_alpha:
cmp al,0x20
je no_catch_special

cmp [location_box],di
jne no_catch

mov word[score_update],1
inc word[score]
call printscore
jmp new_alpha
no_catch:
mov [es:di],ax

call printfail

popa
pop bp
ret 2
no_catch_special:
mov word[fail],0
mov word[fail_update],0
jmp no_catch

timer: 
        pusha

        mov cx,7
        mov si,0
        check_alphabet_time:
        mov dx,word[attribute_alphabets+si]
        mov dl,dh
        mov dh,0

        dec word[speed_alphabets+si]
        cmp word[speed_alphabets+si],0
        jg no_movedown

        push si
        call movedown
        mov [speed_alphabets+si],dx
        no_movedown:
        add si,2
        loop check_alphabet_time

        exit_timer:
        mov al, 0x20
        out 0x20, al ; end of interrupt
        popa
        iret ; return from interrupt



setisr:
push ax
                xor ax, ax
                mov es, ax ; point es to IVT base
                mov ax, [es:9*4]
                mov [oldisr], ax ; save offset of old routine
                mov ax, [es:9*4+2]
                mov [oldisr+2], ax ; save segment of old routine
                cli ; disable interrupts
                mov word [es:9*4], kbisr ; store offset at n*4
                mov [es:9*4+2], cs ; store segment at n*4+2
                sti ; enable interrupts

                xor ax, ax
                mov es, ax ; point es to IVT base
                mov ax, [es:8*4]
                mov [oldisr_timer], ax ; save offset of old routine
                mov ax, [es:8*4+2]
                mov [oldisr_timer+2], ax ; save segment of old routine
                cli ; disable interrupts
                mov word [es:8*4], timer; store offset at n*4
                mov [es:8*4+2], cs ; store segment at n*4+2
                sti ; enable interrupts

pop ax
ret

removeisr:
push ax
xor ax, ax

                mov es, ax ; point es to IVT base
                cli ; disable interrupts
                mov ax,[oldisr]
                mov word [es:9*4], ax ; store offset at n*4
                mov ax,[oldisr+2]
                mov [es:9*4+2], ax; store segment at n*4+2
                sti ; enable interrupts

                cli ; disable interrupts
                mov ax,[oldisr_timer]
                mov word [es:8*4], ax ; store offset at n*4
                mov ax,[oldisr_timer+2]
                mov [es:8*4+2], ax; store segment at n*4+2
                sti ; enable interrupts

pop ax
ret
;-----------------

generate_alphabet:
push bp
mov bp,sp
pusha

mov di,[bp+4]

push 160
call RANDGEN
TEST dx,1
je skip_make_even
dec dx
skip_make_even:
add dx,160;1strow
mov [location_alphabets+di],dx

push 4
call RANDGEN
add dx,3;minimum3ticks
mov [speed_alphabets+di],dx

mov ah,dl
push 26;totalalpha
call RANDGEN
mov al,'A'
add al,dl ;RandomGenerate
mov [attribute_alphabets+di],ax

popa
pop bp
ret 2
;-----------
startgame:
pusha
mov ax,0xb800
mov es,ax
mov di,[location_box]
mov word[es:di],0x07DC

 mov ah, 0x13 ; service 13 - print string
 mov al, 0 ;
 mov bh, 0 ; output on page 0
 mov bl, 7 ; normal attrib
 mov dx, 0x0000;rowcol
 mov cx, 7 ; length of string
 push cs
 pop es ; segment of string
 mov bp, score_string ; offset of string
 int 0x10 ; call BIOS video service

 mov ah, 0x13 ; service 13 - print string
 mov al, 0 ;
 mov bh, 0 ; output on page 0
 mov bl, 7 ; normal attrib
 mov dh,0;row
 mov dl,62;col
 mov cx, 14 ; length of string
 push cs
 pop es ; segment of string
 mov bp, fail_string; offset of string
 int 0x10 ; call BIOS video service


 call printscore
 call printfail

popa
RET

printscore:
 push 7*2
 push word[score]
 call printnum
 RET
 printfail:
 cmp word[fail_update],0
 je no_print
 mov word[fail_update],0
 push 74*2
 push word[fail]
 call printnum
 cmp word[fail],10
 jl no_print
 mov word[gameover],1
 no_print:
 ret

;----
endscreen:
mov ah, 0x13 ; service 13 - print string
 mov al, 0 ;
 mov bh, 0 ; output on page 0
 mov bl, 7 ; normal attrib
 mov dh,12;row
 mov dl,0;col
 mov cx, 24 ; length of string
 push cs
 pop es ; segment of string
 mov bp, gameover_string; offset of string
 int 0x10 ; call BIOS video service

 push 1968
 push word[score]
 call printnum

 ret
;-----
start:
call clrscr
call setisr
call startgame
c1:
cmp word[gameover],1
jnz c1

call removeisr
call clrscr
call endscreen

mov ah,0
int 0x16
call clrscr

mov ax,0x4c00
int 0x21
