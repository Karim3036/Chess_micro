.model small
.stack 64
.data
king_offset equ 15  ;king's index is 15
current_pos db ?
Screen_Width equ 320
Possible_moves dw 22 dup('?')
flag db 0

.code

PushALL MACRO
    Push AX
    Push BX
    Push CX
    Push DX
    Push DI
ENDM PushALL

PopALL MACRO
    Pop DI
    Pop DX
    Pop CX
    Pop BX
    Pop Ax
ENDM PopALL

Clear MACRO 
    mov Ax,3
    int 10h
ENDM Clear  

king_limitations proc far 

    PushALL
    xor ax,ax
    mov ax,Screen_Width	;Y*ScreenWidth + X
	mul dx
	add ax,cx
	mov di,ax
    LEA SI,Possible_moves
    LEA BX,flag

    sub di,22 ;to the left
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    ;(if not put in possible moves else jmp over it)
    mov ax,[BX]
    cmp ax,0
    je next1
    mov [SI],di
    add si,2
    next1:

    sub di,320 ;top left
     ;we need to check for it not to pass the boundaries 
      call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
    mov ax,[BX]
    cmp ax,0
    je next2
    mov [SI],di
    add si,2
    next2:

    add di,22 ;top 
    ;we need to check for it not to pass the boundaries 
     call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
    mov ax,[BX]
    cmp ax,0
    je next3
    mov [SI],di
    add si,2
    next3:

    add di,22 ;top right
    ;we need to check for it not to pass the boundaries 
     call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
     mov ax,[BX]
     cmp ax,0
     je next4
     mov [SI],di
     add si,2
     next4:

     add di,320 ; right
    ;we need to check for it not to pass the boundaries 
     call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
     mov ax,[BX]
     cmp ax,0
     je next5
     mov [SI],di
     add si,2
     next5:

     add di,320 ;down right
    ;we need to check for it not to pass the boundaries 
     call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
     mov ax,[BX]
     cmp ax,0
     je next6
     mov [SI],di
     add si,2
     next6:

     sub di,22 ;down 
    ;we need to check for it not to pass the boundaries 
     call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
      mov ax,[BX]
     cmp ax,0
     je next7
     mov [SI],di
     add si,2
     next7: 
     sub di,22 ;down left
    ;we need to check for it not to pass the boundaries 
     call Check_In_boundaries
     ;(if not put in possible moves else jmp over it)
     mov ax,[BX]
     cmp ax,0
     je next8
     mov [SI],di
     add si,2
     next8:
     
    PopALL
king_limitations endp

Draw_Square Macro Pos
LOCAL back
PushALL
mov di,Pos
call Get_coordinates
mov al,0eh
mov ah,0ch
back: int 10h
inc cx
cmp cx,22
jnz back
PopALL
Draw_Square ENDM


Get_coordinates proc far ;get cx and dx from DI
PushALL

;DI
mov dx,0
mov ax,DI
dloop1: 
cmp ax,320
jb getx
sub ax,320
inc dx
jmp dloop1
getx: mov cx,ax

PopALL
Get_coordinates endp


Check_In_boundaries proc far
PushALL
LEA BX,flag
call Get_coordinates

cmp cx,75 ;if left
jb outofboundaies
cmp cx,229 ;if right
ja outofboundaies
cmp dx,3 ;if above
jb outofboundaies
cmp dx,157 ;if below
ja outofboundaies 
jmp exitfn
 outofboundaies:
 ;flaaaaaaaaaaaaaaaaaaaag
 mov [BX],0
 exitfn:
PopALL
Check_In_boundaries endp