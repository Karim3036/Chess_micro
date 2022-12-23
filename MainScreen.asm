
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

.Model Small
.Stack 64
.Data

    enter_name      db 'Please Enter your name:','$'
    Player_name     db  15,?,15 DUP('$') 
    cont            db 'Press any key to continue','$'
    main_menu_chat  db 'To start chatting press F3','$'
    main_menu_game  db 'To start the game press F2','$'
    main_menu_exit  db 'To end the program press ESC','$'
    Error_Message   db 'Error! Name must start with a letter','$'
    Waiting_Message db 'Invitation sent, wait for response','$'

    side_limit dw 0
    flag       db 0

    ;; Constants for drawing
    Screen_Width		    equ 320
    Screen_Heigth	 	    equ 200
    
    Sprite_Width	 	    equ 8
    Sprite_Height	 	    equ 5
    squarewidth             equ 21
    Num_of_Supported_Chars	equ 36

    ;********************* arrays are arranged from the top of the board to the bottom **********************************;
    ;*********************    Black pieces, black pawns, white pawns, white pieces     **********************************;

    black_x_pos dw 75,229,97,207,119,185,141,163,75,97,119,141,163,185,207,229 
    black_y_pos dw 8 dup(3),8 dup(25)

    white_x_pos dw 75,97,119,141,163,185,207,229,75,229,97,207,119,185,141,163             
    white_y_pos dw 8 dup(135),8 dup(157)

    Background_colors_Row dw 3,8,3,8,3,8,3,8, 8,3,8,3,8,3,3,8

    ;;  Sprite Bitmaps
    Square          dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b
                    dw 1111111111111111b

    Rook	    	dw 0111101111011110b
                    dw 0111101111011110b
                    dw 0111101111011110b
                    dw 0111111111111110b
                    dw 0001111111111000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0011111111111100b
                    dw 0011111111111100b
                    dw 0111111111111110b

    Knight			dw 0000110011000000b
                    dw 0000111111000000b
                    dw 0001111111110000b
                    dw 0011111111111000b
                    dw 1111100111111000b
                    dw 1111111111111000b
                    dw 0001111111111000b
                    dw 0000111111110000b
                    dw 0000011111100000b
                    dw 0000011111100000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0001111111111000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0011111111111100b

    Bishop			dw 0000001111000000b
                    dw 0000001111000000b
                    dw 0000111110010000b
                    dw 0001111100111000b
                    dw 0011111100111100b
                    dw 0011111001111100b
                    dw 0011111001111100b
                    dw 0011111011111100b
                    dw 0011111111111100b
                    dw 0011111111111100b
                    dw 0001111111111000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0001111111111000b
                    dw 0011111111111100b

    Pawn			dw 0000001111000000b
                    dw 0000011111100000b
                    dw 0000111111110000b
                    dw 0000011111100000b
                    dw 0000001111000000b
                    dw 0000011111100000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0000001111000000b
                    dw 0000001111000000b
                    dw 0000001111000000b
                    dw 0000011111100000b
                    dw 0000011111100000b
                    dw 0000111111110000b
                    dw 0000111111110000b
                    dw 0001111111111000b

    Queen           dw 0110111001110110b
                    dw 1110111001110111b
                    dw 1110111001110111b
                    dw 0110011001100110b
                    dw 0110011001100110b
                    dw 0110011001100110b
                    dw 0110011001100110b
                    dw 0110011001100110b
                    dw 0111111111111110b
                    dw 0111111111111110b
                    dw 0111111111111110b
                    dw 0011111111111100b
                    dw 0001111111111000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0011111111111100b				

    King			dw 0000000110000000b
                    dw 0000000110000000b
                    dw 0000011111100000b
                    dw 0000011111100000b
                    dw 0000000110000000b
                    dw 0001111111111000b
                    dw 0111111111111110b
                    dw 0111000110001110b
                    dw 0111000110001110b
                    dw 0111100110011110b
                    dw 0011100110011100b
                    dw 0011110110111100b
                    dw 0001111111111000b
                    dw 0000111111110000b
                    dw 0001111111111000b
                    dw 0011111111111100b				
    
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

movecursor MACRO STR                              ; moves cursor
    MOV AH,2
    MOV DX,STR
    INT 10H
 ENDM movecursor 

DisplayString MACRO STR                           ; DISPLAYS A STRING
    MOV AH,9H
    MOV DX,OFFSET STR
    INT 21H
 ENDM DisplayString 

ReadString MACRO PromptMessage,str                ; READ A STRING                   
    DisplayString PromptMessage
    movecursor str 
    MOV AH,0AH
    MOV DX,OFFSET Player_name
    INT 21H
 ENDM ReadString

ClearScreen MACRO 
    mov ax,0600h
    mov bh,0
    mov cx,0
    mov dx,184fh
    int 10h
ENDM ClearScreen

SetDelayTime Proc near                            ;delay 50 ms                  
    Timer2:                                  
        mov cx,0C350H                             
        D1: Loop D1
        dec bx
        jnz Timer2
ret
ENDP SetDelayTime			


init_board proc far                            ;Draws all pieces from array
	push ax
	push bx
	push cx
	push dx
	push di
	
	ClearScreen

	mov ax,0A000h	; initialize es = Video mode Memory segment, DI = 0
	mov es,ax

	call chessboard

	lea si,black_x_pos
	lea di,black_y_pos
	mov cx,[si]
	mov dx,[di]
	mov al, 0
    call Draw_Rook
    mov cx,[si]+2
    mov dx,[di]+2
    call Draw_Rook
    mov cx,[si]+4
    mov dx,[di]+4
    call Draw_Knight
    mov cx,[si]+6
    mov dx,[di]+6
    call Draw_Knight
    mov cx,[si]+8
    mov dx,[di]+8
    call Draw_Bishop
    mov cx,[si]+10
    mov dx,[di]+10
    call Draw_Bishop
    mov cx,[si]+12
    mov dx,[di]+12
    call Draw_Queen
    mov cx,[si]+14
    mov dx,[di]+14
    call Draw_King
    mov cx,[si]+16
    mov dx,[di]+16
    call Draw_Pawn
    mov cx,[si]+18
    mov dx,[di]+18
    call Draw_Pawn
    mov cx,[si]+20
    mov dx,[di]+20
    call Draw_Pawn
    mov cx,[si]+22
    mov dx,[di]+22
    call Draw_Pawn
    mov cx,[si]+24
    mov dx,[di]+24
    call Draw_Pawn
    mov cx,[si]+26
    mov dx,[di]+26
    call Draw_Pawn
    mov cx,[si]+28
    mov dx,[di]+28
    call Draw_Pawn
    mov cx,[si]+30
    mov dx,[di]+30
    call Draw_Pawn

	lea si,white_x_pos
    lea di,white_y_pos

	mov cx,[si]
	mov dx,[di]
	mov al, 0fh
    call Draw_Pawn
    mov cx,[si]+2
    mov dx,[di]+2
    call Draw_Pawn
    mov cx,[si]+4
    mov dx,[di]+4
    call Draw_Pawn
    mov cx,[si]+6
    mov dx,[di]+6
    call Draw_Pawn
    mov cx,[si]+8
    mov dx,[di]+8
    call Draw_Pawn
    mov cx,[si]+10
    mov dx,[di]+10
    call Draw_Pawn
    mov cx,[si]+12
    mov dx,[di]+12
    call Draw_Pawn
    mov cx,[si]+14
    mov dx,[di]+14
    call Draw_Pawn
    mov cx,[si]+16
    mov dx,[di]+16
    call Draw_Rook
    mov cx,[si]+18
    mov dx,[di]+18
    call Draw_Rook
    mov cx,[si]+20
    mov dx,[di]+20
    call Draw_Knight
    mov cx,[si]+22
    mov dx,[di]+22
    call Draw_Knight
    mov cx,[si]+24
    mov dx,[di]+24
    call Draw_Bishop
    mov cx,[si]+26
    mov dx,[di]+26
    call Draw_Bishop
    mov cx,[si]+28
    mov dx,[di]+28
    call Draw_Queen
    mov cx,[si]+30
    mov dx,[di]+30
    call Draw_King


	pop di
	pop dx
	pop cx
	pop bx
	pop ax
ret
init_board endp


chessboard proc 
	push ax
	push bx
	push cx
	push dx
	push di

	
	mov di,72 ;;to center the board
	mov bx,4 ;;we: 2 sub loops to make first 2 rows so 4 iterations in BackGndloop to make 8 rows
  
		
	mov ch,0
    
BackGndloop:
   
  mov dl,22
	row1:
	
		mov al, 3	
		mov cl, 22
		rep stosb
		
		mov al, 8
		mov cl, 22
		rep stosb

        mov al, 3
		mov cl, 22
		rep stosb
		
		mov al, 8
		mov cl, 22
		rep stosb

        mov al, 3	
		mov cl, 22
		rep stosb
		
		mov al, 8
		mov cl, 22
		rep stosb

        mov al, 3	
		mov cl, 22
		rep stosb
		
		mov al, 8
		mov cl, 22
		rep stosb
        
        add di,144 ;; to skip the remaining cols in the line as the board is 176x176

		dec dl
	jnz row1

mov dl,22
row2:
        mov al, 8	
		mov cl, 22
		rep stosb
		
		mov al, 3
		mov cl, 22
		rep stosb

        mov al, 8	
		mov cl, 22
		rep stosb
		
		mov al, 3
		mov cl, 22
		rep stosb

        mov al, 8	
        mov cl, 22
		rep stosb
		
		mov al, 3
		mov cl, 22
		rep stosb

        mov al, 8	
		mov cl, 22
		rep stosb
		
		mov al, 3
		mov cl, 22
		rep stosb
        
        add di,144
		dec dl
	jnz row2
dec bx
jnz BackGndLoop
    
	pop di
	pop dx
	pop cx
	pop bx
	pop ax

ret
chessboard endp

Draw_Square proc    ;; Takes x in dl and Y in dh, Al color
    Push di
	call Get_ScreenPos
	push si
	mov si, offset Square

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_Square endp

Draw_King proc 	    ;; Takes x in dl and Y in dh, Al color
    Push di
	call Get_ScreenPos
	push si
	mov si, offset King

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_King endp

Draw_Queen proc     ;; Takes x in dl and Y in dh, Al color
    push di
	call Get_ScreenPos
	push si
	mov si, offset Queen

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_Queen endp

Draw_Rook proc 	    ;; Takes x in dl and Y in dh, Al color
    push di
	call Get_ScreenPos
	push si
	mov si, offset Rook

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_Rook endp

Draw_Knight proc 	    ;; Takes x in dl and Y in dh, Al color
    push di
	call Get_ScreenPos
	push si
	mov si, offset Knight

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_Knight endp

Draw_Pawn proc 	    ;; Takes x in dl and Y in dh, Al color
    push di
	call Get_ScreenPos
	push si
	mov si, offset Pawn

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_Pawn endp	

Draw_Bishop proc 	    ;; Takes x in dl and Y in dh, Al color
    push di
	call Get_ScreenPos
	push si
	mov si, offset Bishop

	call Draw_Sprite
	pop si
    pop di
	ret
Draw_Bishop endp	



Draw_Sprite proc    ;takes X in cx, Y in dx, Si pointing to bitmap of variable, Al = sprite color , Ah = back ground color

	push ax
	push bx
	push cx
	push dx
    push di

	call Get_ScreenPos

	mov dl,al
	mov cx, 16
	
	Drawing_Row1:
	    lodsw			    ;take first row of bitmap of letter in al

	    xchg ax,dx		    	    ;put color in al, and first row of bitmap in dl

	    mov bx,1000000000000000b	    ;for checking if the first bit is one
	    Drawing_Pixels1:
		test dx,bx
		jz BackGND1

		stosb		    	    ;put color in di from al
		jmp Next_Pixel1

		BackGND1:
		;xchg al,ah	    	    ;; To set back ground color too
		;stosb
		;xchg al,ah

		inc di

		Next_Pixel1:
		shr bx,1
	    jnz Drawing_Pixels1

	    add di, 320 - 16
	    xchg ax,dx		    	    ;put color in dl to get it back next loop
	loop Drawing_Row1

    pop di
	pop dx
	pop cx
	pop bx
	pop ax

	ret
Draw_Sprite	endp


Get_ScreenPos	proc		;takes X in cx, Y in dx, returns DI pointing to memory pos of X,Y
	push ax
	push bx
	push cx
	push dx

	xor ax,ax		;setting to zero


	
	mov ax,Screen_Width	;Y*ScreenWidth + X
	mul dx
	add ax,cx

	mov di,ax
    
	pop dx
	pop cx
	pop bx
	pop ax

	ret
Get_ScreenPos	endp

Get_coordinates proc far ;get cx and dx from DI

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

ret
Get_coordinates endp

Check_In_boundaries proc far
    PushALL
    LEA si,flag
    mov bx,0
    call Get_coordinates

    cmp cx,75 ;if left
    jb outofboundaries

    cmp cx,229 ;if right
    ja outofboundaries

    cmp dx,3 ;if above
    jb outofboundaries

    cmp dx,157 ;if below
    ja outofboundaries 

    jmp exitfn

outofboundaries:
    ;flaaaaaaaaaaaaaaaaaaaag
    mov [si],bx

exitfn:
    PopALL
ret
Check_In_boundaries endp

DrawSquareBorder proc far                         ; To call you must set: cx with x position, dx with y, al with the border color

PushALL
    
    LEA SI,side_limit
    ;call Get_coordinates
    sub cx,3
    sub dx,3
PushALL

    mov bx,cx
    add bx,squarewidth
    mov [si],bx
    add si,2

    mov bx,dx
    add bx,squarewidth
    mov [si],bx
    add si,2

    mov bx,cx
    mov [si],bx
    add si,2

    mov bx,dx
    mov [si],bx
PopALL
    LEA SI,side_limit
    ;mov al,0AH                                     
    mov ah,0ch

side1:

    int 10h
    inc cx

    cmp cx,[SI]
    jnz side1
    add si,2

side2:
    int 10h
    inc dx

    cmp dx,[SI]
    jnz side2
    add si,2

side3:
    int 10h
    dec cx

    cmp cx,[SI]
    jnz side3

    add si,2

side4: 
    int 10h
    dec dx

    cmp dx,[SI]
    jnz side4

PopALL
ret
DrawSquareBorder endp

Move proc      
;************** Piece new position is changed in array inside the function, array should enter the function holding the previous position ***************;

    ;al contains the color
    ;bx contains troop index
    ;cx contains x position
    ;dx contains y position

    PushALL
    cmp al,0
    jz Black  
    lea si,white_x_pos
    lea di,white_y_pos
    jmp Continue

Black:
    lea si,black_x_pos
    lea di,black_y_pos

Continue:
    
    mov cx,[si + bx]
    mov dx,[di + bx]
 
    mov ah,0DH
    mov bh,0
    int 10h                             ;Getting the corner point to get the beackground color

    call Draw_Square                    ;Drawing in the old position with the background color

    PopALL

Drawing:   
    cmp al,0
    je Black_Troop

    cmp bl,16
    jL Pawn1

    cmp bl,20
    jL Rook1

    cmp bl,24
    jL Knight1

    cmp bl,28
    jL Bishop1

    cmp bl,30
    jL Queen1

    cmp bl,31
    jL King1

Black_Troop:

    cmp bl,4
    jL Rook1

    cmp bl,8
    jL Knight1

    cmp bl,12
    jL Bishop1

    cmp bl,14
    jL Queen1

    cmp bl,16
    jL King1

Pawn1:
    Call Draw_Pawn
    jmp FnEnd
Rook1:
    Call Draw_Rook
    jmp FnEnd
Knight1:
    Call Draw_Knight
    jmp FnEnd
Bishop1:
    Call Draw_Bishop
    jmp FnEnd
Queen1:
    Call Draw_Queen
    jmp FnEnd
King1:
    Call Draw_King
    jmp FnEnd

FnEnd:
    cmp al,0
    jz Black2  
    lea si,white_x_pos
    lea di,white_y_pos
    jmp Continue2

Black2:
    lea si,black_x_pos
    lea di,black_y_pos
Continue2:

    mov [si + bx],cx                        ;New position added to the array                
    mov [di + bx],dx

ret
ENDP Move  

User_Move proc far
    PushALL
    mov cx,75                               ;Bottom right corner coordinates
    mov dx,157
    mov al,0AH

    call DrawSquareBorder

    lea si,white_x_pos
    lea di,white_y_pos


ret
endp User_Move

MainScreen PROC FAR

    NameEntry:
         movecursor 0104h
         ReadString enter_name,0204h
         mov si,offset Player_name
         add si,2

         mov bx,64d                               ;Checking first letter of the name, if capital letter
         cmp [si],bl
         ja check2

         ClearScreen
         DisplayString Error_Message

         mov bx,100H                              ;100*50ms = delay 5 secs
         call SetDelayTime

         ClearScreen
         jmp NameEntry

    check2: 
        mov bx,91d                         ;Check 2 for the first letter, if small letter
         cmp [si],bl
         jb Checkover

        mov bx,96d
         cmp [si],bl
         ja check22

         ClearScreen
         DisplayString Error_Message 

         mov bx,100H
         call SetDelayTime

         ClearScreen
         jmp NameEntry

    check22:
        mov bx,123d
        cmp [si],bl
        jb Checkover

        ClearScreen
        DisplayString Error_Message

        mov bx,100H                       
        call SetDelayTime

         ClearScreen
        jmp NameEntry

    Checkover:
         movecursor 1405h
         DisplayString cont                        ; displays "press any key to continue"
 
         mov ah,0
         int 16h
    
         ClearScreen

         mov cx,0                                   ;Notification bar line
         mov dx,180
         mov al,0FH
         mov ah,0ch
    horz:
         int 10h
         inc cx
         cmp cx,320
         jnz horz

         movecursor 0404h
         DisplayString main_menu_chat               ; displays "To start chatting press F3"

         movecursor 0604h
         DisplayString main_menu_game               ; displays "To start the game press F2"

         movecursor 0804h
         DisplayString main_menu_exit               ; displays "To end the program press ESC"

         mov ah,0
         int 16h

         cmp ah,3DH                                 ; if F3 is pressed
         jz chat
         
         cmp ah,3CH                                 ; if F2 is pressed
         jz game

         cmp al,27                                  ; if ESC is pressed
         je ProgramEnd
         
         chat:
            movecursor 1704h
            DisplayString Waiting_Message
            jmp MainEnd
         game:
            call init_board

            mov bx,100h                        ;Testing the drawing
            call SetDelayTime 
            
            mov al,0FH
            mov bx,10
            mov cx,185
            mov dx,69
            Call Move
            call User_Move

            mov bx,100h                        ;Testing the drawing
            call SetDelayTime

            mov al,0FH
            mov bx,10
            mov cx,141
            mov dx,69
            Call Move

            jmp MainEnd

ProgramEnd:
        ClearScreen
        mov ah,04ch
        int 21h
MainEnd:
ret
MainScreen ENDP

MAIN Proc far
    mov ax,@data
    mov ds,ax
    
    mov ah,0
    mov al,13h
    int 10h                                      ; change to video mode

    Call MainScreen

Main ENDP
End main
