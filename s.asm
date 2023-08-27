[org 0x0100]
jmp start

empty: db 30
      db 0
      times 30 db 0
buf : times 2000 db 0


dcount : dw 0 
handler: dw 0
msg1: db '>>BOULDER DASH NUCES EDITION<<<'

msg2: db 'File not found'

seconds: dw 0 
timerflag: dw 0
oldkb: dd 0 
;;;;; COPY LINES 007-047 FROM EXAMPLE 9.7 (printnum) ;;;;; 
; keyboard interrupt service routine 
kbisr: 
	  push ax 
	 push es 
	 mov ax, 0xb800 
 	 mov es, ax 
	 in al, 0x60 ; read char from keyboard port 
	 mov bx,[cs:timerflag]
	 cmp al, 0x4b ; has the left arrow pressed 
	 je lefttt ; no, try next comparison

	  cmp al, 0x4d ; has the left arrow pressed 
	 je righttt ; no, try next comparison

	  cmp al, 0x48 ; has the up arrow pressed 
	 je upp ; no, try next comparison 

	 	 cmp al, 0xd0 ; has the down key released 
	 	 je downn ; no, chain to old ISR 

	 	 jmp exitt

lefttt:
	cmp byte[es:di-2],0xDB
	je exitt
	cmp byte[es:di-2],0x09
	je exitt
mov cl,byte[es:di-2]	
	mov ax,0 
	mov di,bx
	mov [es:di] : ax
	sub bx ,2 
	 jmp exitt
righttt:
	cmp byte[es:di+2],0xDB
	je exitt
	cmp byte[es:di+2],0x09
	je exitt
	mov cl,byte[es:di+2]
	cmp byte[es:di],0x04
	mov ax,0 
	mov di,bx
	mov [es:di] : ax
	add bx ,2 
	 jmp exitt
upp:
	cmp byte[es:di-160],0xDB
	je exitt
mov cl,byte[es:di-160]
	mov ax,0 
	mov di,bx
	mov [es:di] : ax
	sub bx ,160
	 jmp exitt
downn:
	cmp byte[es:di+160],0xDB
	je exitt
	cmp byte[es:di+160],0x09
	je exitt
mov cl,byte[es:di+160]
	mov ax,0 
	mov di,bx
	mov [es:di] : ax
	add bx ,160
	 jmp exitt

exitt: 
	mov word [cs:timerflag],bx
	mov di,word[cs:timerflag]
	cmp cl,0x04
	je dplus
back:
	mov dl ,byte[es:di-160]

	cmp byte[es:di],0x7F
	 je gameover
	 mov al,0x02
    mov ah,10100000b
	 mov [es:di] : ax
	 cmp dl,0x09
	 	je terrr
	 mov al, 0x20 
	 out 0x20, al ; send EOI to PIC 
	pop es
	 pop ax 
	 iret ; return from interruptturn from interrupt 

dplus:
	inc word[cs:dcount]
	mov cl,0
	jmp back
 gameover:
 	mov byte[es:200],'U'
   mov byte[es:202],' '
    mov byte[es:204],'R'
     mov byte[es:206],' '
      mov byte[es:208],'S'
       mov byte[es:210],'A'
        mov byte[es:212],'F'
   mov byte[es:214],'E'
   jmp terminate

 terrr:
 mov byte[es:200],'G'
   mov byte[es:202],'A'
    mov byte[es:204],'M'
     mov byte[es:206],'E'
      mov byte[es:208],'O'
       mov byte[es:210],'V'
        mov byte[es:212],'E'
   mov byte[es:214],'R'
 	jmp terminate
timer: 
	 push ax 

	 cmp word[cs:timerflag], 0 ; is the printing flag set 
	 je skipall ; no, leave the ISR 
	 inc word [cs:seconds] ; increment tick count 
	 push word [cs:seconds] 
	 call printnum ; print tick count 
skipall: 
	 mov al, 0x20 
	 out 0x20, al ; send EOI to PIC 
	 pop ax 
	 iret

printnum: 
	 push bp 
	 mov bp, sp 
	 push es 
	 push ax 
	 push bx 
	 push cx 
	 push dx 
	 push di 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov ax, [bp+4] ; load number in ax 
	 mov bx, 10 ; use base 10 for division 
	 mov cx, 0 ; initialize count of digits 
nextdigit: 
	 mov dx, 0 ; zero upper half of dividend 
	 div bx ; divide by 10 
	 add dl, 0x30 ; convert digit into ascii value 
	 push dx ; save ascii value on stack 
	 inc cx ; increment count of values 
	 cmp ax, 0 ; is the quotient zero 
	 jnz nextdigit ; if no divide it again 
	 mov di, 140 ; point di to 70th column 
nextpos: 
	 pop dx ; remove a digit from the stack 
	 mov dh, 0x07 ; use normal attribute 
	 mov [es:di], dx ; print char on screen 
	 add di, 2 ; move to next screen location 
	 loop nextpos ; repeat for all digits on stack
	
	 
mov byte[es:di],'s'
	 mov byte[es:130],'T'
   mov byte[es:132],'i'
   mov byte[es:134],'m'
    mov byte[es:136],'e'
     mov byte[es:138],':'

      mov byte[es:30],'S'
   mov byte[es:32],'c'
   mov byte[es:34],'o'
    mov byte[es:36],'r'
     mov byte[es:38],'e'
mov byte[es:40],':'
     mov di,42
     mov ax,word[cs:dcount]
     mov bx, 10 ; use base 10 for division 
	 mov cx, 0 ; initialize count of digits 
nextdigit1: 
	 mov dx, 0 ; zero upper half of dividend 
	 div bx ; divide by 10 
	 add dl, 0x30 ; convert digit into ascii value 
	 push dx ; save ascii value on stack 
	 inc cx ; increment count of values 
	 cmp ax, 0 ; is the quotient zero 
	 jnz nextdigit1 ; if no divide it again 
	 mov di, 42 ; point di to 70th column 
nextpos1: 
	 pop dx ; remove a digit from the stack 
	 mov dh, 0x07 ; use normal attribute 
	 mov [es:di], dx ; print char on screen 
	 add di, 2 ; move to next screen location 
	 loop nextpos1 ; repeat for all digits on stack
	 pop di 
	 pop dx 
	 pop cx 
	 pop bx 

	 pop ax
	 pop es 
 	pop bp 
 	ret 2

ms1:
     push es
           push ax
           push bx
           push cx
           push dx
           push si
           push di
 mov ax,0xb800
   mov es,ax

 mov byte[es:70],'>'
   mov byte[es:72],'B'
   mov byte[es:74],'O'
    mov byte[es:76],'U'
     mov byte[es:78],'L'
      mov byte[es:80],'D'
       mov byte[es:82],'E'
        mov byte[es:84],'R'
   mov byte[es:86],' '
   mov byte[es:88],'D'
    mov byte[es:90],'A'
     mov byte[es:92],'S'
      mov byte[es:94],'H'
       mov byte[es:96],'<'
       
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
ret


printstr:
      
        push bp
           mov bp,sp
           push es
           push ax
           push bx
           push cx
           push dx
           push si
           push di
 mov ax,0xb800
   mov es,ax
   mov si,[bp+6]
   mov cx,[bp+4]
   mov ah,07h
   mov di,0
 cld
 next:
     lodsb
     stosw
     loop next

pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 4


msg6_a:
           push es
           push ax
           push bx
           push cx
           push dx
           push si
           push di
 mov ax,0xb800
   mov es,ax

   mov byte[es:200],'N'
   mov byte[es:202],'O'
    mov byte[es:204],' '
     mov byte[es:206],'F'
      mov byte[es:208],'I'
       mov byte[es:210],'L'
        mov byte[es:212],'E'
   mov byte[es:214],' '
   mov byte[es:216],'F'
    mov byte[es:218],'O'
     mov byte[es:220],'U'
      mov byte[es:222],'N'
       mov byte[es:224],'D'
        mov byte[es:226],' '

   mov byte[es:228],'-'
   mov byte[es:230],'('
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
jmp terminate
clrscr:
        push es
       push ax
       push cx
       push di

 mov ax,0xb800
 mov es,ax
 xor di,di
 mov ax,0x720
 mov cx,2000

 cld
 rep stosw

 pop di
 pop cx
 pop ax
 pop es

ret

print_file:
      
           push es
           push ax
           push bx
           push cx
           push dx
           push si
           push di
 mov ax,0xb800
   mov es,ax

   mov byte[es:200],'E'
   mov byte[es:202],'N'
    mov byte[es:204],'T'
     mov byte[es:206],'E'
      mov byte[es:208],'R'
       mov byte[es:210],' '
        mov byte[es:212],'F'
   mov byte[es:214],'I'
   mov byte[es:216],'L'
    mov byte[es:218],'E'
     mov byte[es:220],' '
      mov byte[es:222],'N'
       mov byte[es:224],'A'
        mov byte[es:226],'M'

   mov byte[es:228],'E'
   mov byte[es:230],':'
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es

ret 


printgamemap:
	push bp

	mov bp, sp
	push es
	push ax

	push cx
	push si
	push di
	push ds
	pop es
	mov di, [bp+4]
	mov cx, 0xffff
	mov al, [null]
	repne scasb
	mov ax, 0xffff
	sub ax, cx
	dec ax
	jz exit
	mov cx, ax
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte [bp+8]
	add ax, [bp+10]
	shl ax, 1
	mov di,ax
	mov si, [bp+4]
	mov ah, [bp+6]
	cld
	nextchar: 
	mov al,[si]
	add si,1
	cmp al,0x57
	je print_wall

	cmp al,0x78
	je print_dirt

	cmp al,0x42
	je print_mann

	cmp al,0x44
	je print_d
	cmp al,0x54
	je exit_point
	cmp al,0x52
	je print_hurdle

	print_schar:
	                mov [es:di],ax
	               add di,2
	loop nextchar
	exit: pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
print_d:
   mov al,0x04
   mov ah,00110001b
jmp print_schar
 
print_mann:
   mov al,0x09
   mov ah,01100000b
jmp print_schar 
print_dirt:
   mov al,0xB1
   mov ah,01100111b
jmp print_schar


print_wall:

	mov word [cs:timerflag], 1602
   mov al,0xDB
   mov ah,01010100b
jmp print_schar
 

exit_point:
	
    mov al,0x7F
    mov ah,10000111b

jmp print_schar

print_hurdle:
	
	
    mov al,0x02
    mov ah,10100000b
jmp print_schar

gameborder: 
	push ax
	push es
	push cx
	push dx
	push si
	push di

	mov di,320


	mov ax,0xb800
	mov es,ax

	mov al,0xDB
	mov ah,01110111b
	mov cx,80
	ll1:
	mov [es:di],ax
	add di,2
	loop ll1

	mov di,320


	mov al,0xDB
	mov ah,01110111b
	mov cx,22
	ll2:
	mov [es:di],ax
	add di,158
	mov [es:di],ax
	add di,2
	loop ll2

	mov di,3680
	mov cx,79
	ll3:
	mov [es:di],ax
	add di,2
	loop ll3

	mov di,3840
	mov cx,80
	mov ah,07h
	ll4:
	mov [es:di],ax
	add di,2
	loop ll4





	pop di
	pop si
	pop dx
	pop cx
	pop es
	pop ax
	ret


null: db '0'



start:
call clrscr
call ms1
call print_file
mov ah,0Ah
    mov dx,empty
   int 21h
   mov ah,3Dh       
   mov al,0x00       

mov bx,0
mov bl,[empty+1]
add bl,2
mov bp,0
mov bp,bx
mov byte[empty+bp],0
mov dx,empty+2

int 21h
jc msg6_a
 
mov word[handler],ax

mov ah,3Fh
                     
                 
mov dx,buf
mov cx,2000
mov bx,[handler]
int 21h


mov ah,3Eh
mov bx,[handler]
int 21h

call clrscr



push 1
push 3

push 01110111b

mov ax, buf
push ax
call printgamemap

call gameborder

call ms1

 ; set flag to start printing
 xor ax, ax 
 mov es, ax ; point es to IVT base 
 mov ax, [es:9*4] 
 mov [oldkb], ax ; save offset of old routine 
 mov ax, [es:9*4+2] 
 mov [oldkb+2], ax ; save segment of old routine 
 cli ; disable interrupts 
 mov word [es:9*4], kbisr ; store offset at n*4 
 mov [es:9*4+2], cs ; store segment at n*4+2 
 mov word [es:8*4], timer ; store offset at n*4 
 mov [es:8*4+2], cs ; store segment at n*4+ 
 sti ; enable interrupts 
 mov dx, start ; end of resident portion 
 add dx, 15 ; round up to next para 
 mov cl, 4 
 shr dx, cl ; number of paras 
terminate:
 mov ax, 0x3100 ; terminate and stay resident 
 int 0x21
