
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

.Model Compact
.Stack 64
.Data

    Questionmark_ascii equ '?'

    Valid                   EQU 1
    Invalid                 EQU 0

    enter_name      db 'Please Enter your name:','$'
    Player_name     db  15,?,15 DUP('$') 
    cont            db 'Press any key to continue','$'
    main_menu_chat  db 'To start chatting press F3','$'
    main_menu_game  db 'To start the game press F2','$'
    main_menu_exit  db 'To end the program press ESC','$'
    Error_Message   db 'Error! Name must start with a letter','$'
    Waiting_Message db 'Invitation sent, wait for response','$'

    side_limit                  dw 4 dup(?)
    Possible_move_boarder       dw 4 dup(?)
    Possible_moves_Size         dw 0
    Possible_moves              dw 21 dup(?),'$'    ;Max possible moves (for Queen) = Queen

    Capture_Opponent_array      dw 21 dup(?)        ;********************************************************************************************

    Opponent_Troop_Index  dw ?
    selected_Troop_Index  dw ?

    Previous_Selection    dw ?  

    Boundaries_flag       dw Valid   
    Ally_flag             dw Valid
    opponent_flag         dw Invalid

    ;; Constants for drawing
    Screen_Width		    equ 320
    Screen_Heigth	 	    equ 200
    
    Sprite_Width	 	       equ 8
    Sprite_Height	 	       equ 5
    squarewidth                equ 21
    Possible_move_boarder_size equ 19
    Num_of_Supported_Chars	   equ 36

    ;********************* arrays are arranged from the top of the board to the bottom **********************************;
    ;*********************    Black pieces, black pawns, white pawns, white pieces     **********************************;

    Player_Pieces_Color dw ?
    White_Pieces        EQU 0
    Black_Pieces        EQU 1

    black_x_pos  dw 75,229,97,207,119,185,141,163,75,97,119,141,163,185,207,229 
    black_y_pos  dw 8 dup(3),8 dup(25)
    black_DI_val dw 1035,1189,1057,1167,1079,1145,1101,1123,8075,8097,8119,8141,8163,8185,8207,8229

    white_x_pos  dw 75,97,119,141,163,185,207,229,75,229,97,207,119,185,141,163             
    white_y_pos  dw 8 dup(135),8 dup(157)
    white_DI_val dw 43275,43297,43319,43341,43363,43385,43407,43429,50315,50469,50337,50447,50359,50425,50381,50403

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

DisplayString MACRO STR
    PushALL
    mov Ah,09h
    mov dx, offset STR
    int 21h    
    PopALL
ENDM DisplayString

PushALL MACRO
    Push AX
    Push BX
    Push CX
    Push DX
    Push DI
    Push SI
ENDM PushALL

PopALL MACRO
    Pop SI
    Pop DI
    Pop DX
    Pop CX
    Pop BX
    Pop Ax
ENDM PopALL

movecursor MACRO STR                              ; moves cursor
PushALL
    MOV AH,2
    MOV DX,STR
    INT 10H
PopALL    
 ENDM movecursor 

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

full_board proc far
    PushALL
    mov ax,0A000h
    mov es,ax
    mov di,320
    mov al,6
    mov cx,64000
    rep stosb
    PopALL
ret
full_board endp

init_board proc far                            ;Draws all pieces from array
	push ax
	push bx
	push cx
	push dx
	push di
	
	ClearScreen

	mov ax,0A000h	; initialize es = Video mode Memory segment, DI = 0
	mov es,ax

    mov di,0
    mov al,05h
    mov cx,56320    ; draw background
    rep stosb

    mov di,56320
    mov al,0fh
    mov cx,320    ; draw status bar
    rep stosb

	call chessboard

	lea si,black_x_pos
	lea di,black_y_pos
	mov cx,[si]
	mov dx,[di]
	mov al,0
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

DrawSquareBorder proc far                         ; To call you must set: cx with x position, dx with y, al with the border color

PushALL
    
    LEA SI,side_limit
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

Draw_possiblemove_Border proc far                         ; To call you must set: cx with x position, dx with y, al with the border color

PushALL
    
    LEA SI,Possible_move_boarder
    sub cx,2
    sub dx,2
PushALL

    mov bx,cx
    add bx,Possible_move_boarder_size
    mov [si],bx
    add si,2

    mov bx,dx
    add bx,Possible_move_boarder_size
    mov [si],bx
    add si,2

    mov bx,cx
    mov [si],bx
    add si,2

    mov bx,dx
    mov [si],bx
PopALL
    LEA SI,Possible_move_boarder
    ;mov al,0AH                                     
    mov ah,0ch

side1_possible:

    int 10h
    inc cx

    cmp cx,[SI]
    jnz side1_possible
    add si,2

side2_possible:
    int 10h
    inc dx

    cmp dx,[SI]
    jnz side2_possible
    add si,2

side3_possible:
    int 10h
    dec cx

    cmp cx,[SI]
    jnz side3_possible

    add si,2

side4_possible: 
    int 10h
    dec dx

    cmp dx,[SI]
    jnz side4_possible

PopALL
ret
Draw_possiblemove_Border endp

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

Check_opponent proc far      ;see if there is an opponent troop
                          ;di = next position
    PushALL
    mov ax,Invalid
    mov opponent_flag,ax

    mov ax,Black_Pieces
    cmp ax,Player_Pieces_Color
    jz white_opponent

    lea si,black_DI_val
    jmp opponent_Continue

white_opponent:
    lea si,white_DI_val

opponent_Continue:
    ;mov ax,15            ;Counter for array
    mov bx,0        ;Index for array

Start_the_check2:

    cmp di,[si]
    jz found_opponent

    add bx,2
    add si,2
    cmp bx,30
    ja FnEnd3
    jmp Start_the_check2

found_opponent:
    mov ax,Valid
    mov opponent_flag,ax    
    mov Opponent_Troop_Index,bx 
    jmp FnEnd3

FnEnd3:
    PopALL
ret
Check_opponent endp



Check_ally proc far      ;see if there is a friendly troop
                          ;di = next position
    PushALL
    mov ax,Black_Pieces
    cmp ax,Player_Pieces_Color
    jz Black_Ally
    lea si,white_DI_val
    jmp Ally_Continue

Black_Ally:
    lea si,black_DI_val

Ally_Continue:
    ;mov ax,15            ;Counter for array
    mov bx,0        ;Index for array

Start_the_check: ;***********************************  [si] is replaced by white[bx] 

    cmp di,[si]
    jz found

    add bx,2
    add si,2
    cmp bx,30
    ja finish 
    jmp Start_the_check
found:
    mov ax,Valid
    jmp FnEnd2
finish:
    mov ax,Invalid

FnEnd2:
    lea si,Ally_flag
    mov [si],ax
;Opponent_Exists:
    ;PopALL    
    PopALL
ret
Check_ally endp


Get_coordinates proc far            ;get cx and dx from DI
; Changes in ax after the function
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
    LEA si,Boundaries_flag                
    mov bx,Invalid
    call Get_coordinates

    cmp cx,75 ;if left
    jb outofboundaries

    cmp cx,229 ;if right
    ja outofboundaries

    cmp dx,3 ;if above
    jb outofboundaries

    cmp dx,157 ;if below
    ja outofboundaries 

    jmp InBoundaries

InBoundaries:
    mov bx,Valid

outofboundaries:
    mov [si],bx
    PopALL
ret
Check_In_boundaries endp

Delete_selection proc far
    PushALL

    LEA SI,Possible_moves

    Delete_next:

    mov DI,[SI]

    call Get_coordinates

    mov ah,0DH
    mov bh,0
    int 10h
    
    call Draw_possiblemove_Border              ;Deleting previous border
    
    mov ax,Questionmark_ascii
    mov [SI],ax
    add SI,2
    dec Possible_moves_Size
    jnz Delete_next

    PopALL
ret
Delete_selection endp

king_limitations proc far 

    PushALL
    call Get_ScreenPos
    lea SI,Possible_moves
    mov bx,0

    sub di,22 ;to the left
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next1

    call Check_ally
    cmp Ally_flag,Valid 
    jz next1

    ; No check opponent for king as the king only has 1 possible move in each direction

    ;(if not put in possible moves else jmp over it)
    ;mov ax,[BX]
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx
next1:
    sub di,7040 ;top left
     ;we need to check for it not to pass the boundaries

    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next2

    call Check_ally
    cmp Ally_flag,valid
    jz next2
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next2:
    add di,22 ;top 
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next3

    call Check_ally
    cmp Ally_flag,valid
    jz next3
     ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx
next3:
    add di,22 ;top right
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next4

    call Check_ally
    cmp Ally_flag,valid
    jz next4
    ;(if not put in possible moves else jmp over it)

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next4:
    add di,7040 ; right
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next5

    call Check_ally
    cmp Ally_flag,valid
    jz next5
    ;(if not put in possible moves else jmp over it)

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next5:
    add di,7040 ;down right
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next6

    call Check_ally
    cmp Ally_flag,valid
    jz next6
    ;(if not put in possible moves else jmp over it)

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next6:
    sub di,22 ;down 
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next7

    call Check_ally
    cmp Ally_flag,valid
    jz next7
    ;(if not put in possible moves else jmp over it)

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next7: 
    sub di,22 ;down left
    ;we need to check for it not to pass the boundaries 
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next8

    call Check_ally
    cmp Ally_flag,valid
    jz next8
    ;(if not put in possible moves else jmp over it)

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next8:
    mov Possible_moves_Size,bx
    PopALL
    ret
king_limitations endp

Rook_limitations proc far
    PushALL

    call Get_ScreenPos       ;Returns di
    lea SI,Possible_moves       ;mov di to the array if valid move
    mov bx,0    ;Possible moves array size
    lea bp,Capture_Opponent_array
    push di

horizontal_right:
    add di,22

    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di          ; Position out of right boundaries 

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di            ;Possible moves array
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Add_Troop_To_Capture
    
    jmp horizontal_right

Add_Troop_To_Capture:
    PushALL
    mov ax,Opponent_Troop_Index
    mov [bp],ax
    add bp,2
    PopALL

Get_di:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    ;PopALL    
    pop di
    push di

horizontal_left:
    sub di,22
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di2          ; Position out of left boundaries

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di2

    call Get_coordinates
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Add_Troop2_To_Capture

    jmp horizontal_left

Add_Troop2_To_Capture:
    PushALL
    mov ax,Opponent_Troop_Index
    mov [bp],ax
    add bp,2
    PopALL

Get_di2:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    ;PopALL
    pop di
    push di

vertical_back:
    sub di,7040
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di3          ; Position out of upper boundaries

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di3

    call Get_coordinates
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Add_Troop3_To_Capture
    
    jmp vertical_back

Add_Troop3_To_Capture:
    Add_Troop3_To_Capture:
    PushALL
    mov ax,Opponent_Troop_Index
    mov [bp],ax
    add bp,2
    PopALL

Get_di3:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    ;PopALL
    pop di
    push di

vertical_for:
    add di,7040
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz endit          ; Position out of Lower boundaries

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz endit

    call Get_coordinates
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Add_Troop4_To_Capture

    jmp vertical_for

Add_Troop4_To_Capture:
    Add_Troop4_To_Capture:
    PushALL
    mov ax,Opponent_Troop_Index
    mov [bp],ax
    add bp,2
    PopALL

endit:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    mov Possible_moves_Size,bx
    
    pop di
    PopALL
    ret
Rook_limitations endp

Knight_limitations proc far
PushALL

    call Get_ScreenPos
    LEA SI,Possible_moves
    mov bx,0
    
    push di
    
    add di,7084
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next1_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next1_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next1_knight:

    pop di
    push di

    add di,14102
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next2_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next2_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next2_knight:

    pop di
    push di

    add di,14058
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next3_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next3_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next3_knight:

    pop di
    push di

    add di,6996
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next4_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next4_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next4_knight:

    pop di
    push di
    
    sub di,7084
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next5_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next5_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next5_knight:

    pop di
    push di
    
    sub di,14102
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next6_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next6_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next6_knight:
    
    pop di
    push di
    
    sub di,14058
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next7_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next7_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next7_knight:
    pop di
    push di
    
    sub di,6996
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next8_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next8_knight

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next8_knight:
    mov Possible_moves_Size,bx
    pop di
    PopALL
ret
Knight_limitations endp

Bishop_limitations proc far
PushALL

    call Get_ScreenPos
    LEA SI,Possible_moves
    mov bx,0
    
    Push di
Top_right:
    sub DI,7018
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di1_bishop

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di1_bishop

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di1_bishop

    jmp Top_right

Get_di1_bishop:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

Top_left:
    sub DI,7062
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di2_bishop

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di2_bishop

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di2_bishop

    jmp Top_left

Get_di2_bishop:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di
    
Bottom_right:
    add DI,7062
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di3_bishop

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di3_bishop

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di3_bishop

    jmp Bottom_right

Get_di3_bishop:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

Bottom_left:
    add DI,7018
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz end_it_bishop
    call Check_ally
    cmp Ally_flag,Valid 
    jz end_it_bishop

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz end_it_bishop

    jmp Bottom_left

end_it_bishop:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    mov Possible_moves_Size,bx
    pop di

PopALL
ret
Bishop_limitations endp

Queen_limitations proc far
PushALL
    ;same as bishop
    call Get_ScreenPos
    LEA SI,Possible_moves
    mov bx,0

    Push di

Top_right_Queen:
    sub DI,7018
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di1_Queen

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di1_Queen

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di1_Queen

    jmp Top_right_Queen

Get_di1_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

Top_left_Queen:
    sub DI,7062
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di2_Queen

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di2_Queen

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di2_Queen

    jmp Top_left_Queen

Get_di2_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di
    
Bottom_right_Queen:
    add DI,7062
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di3_Queen

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di3_Queen

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di3_Queen
    
    jmp Bottom_right_Queen

Get_di3_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

Bottom_left_Queen:
    add DI,7018
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di4_Queen

    call Check_ally
    cmp Ally_flag,Valid 
    jz Get_di4_Queen

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di4_Queen
    
    jmp Bottom_left_Queen

    ;same as rook-------------

Get_di4_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

horizontal_right_Queen:
    add di,22
   
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di5_Queen         ; Position out of right boundaries 

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di5_Queen

    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di            ;Possible moves array
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di5_Queen
    
    jmp horizontal_right_Queen

Get_di5_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

horizontal_left_Queen:
    sub di,22
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di6_Queen          ; Position out of left boundaries

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di6_Queen

    call Get_coordinates
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di6_Queen
    
    jmp horizontal_left_Queen

Get_di6_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

vertical_back_Queen:
    sub di,7040
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di7_Queen          ; Position out of upper boundaries

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di7_Queen

    call Get_coordinates
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di7_Queen
    
    jmp vertical_back_Queen

Get_di7_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,ax
    pop di
    push di

vertical_for_Queen:
    add di,7040
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz endit_Queen          ; Position out of left boundaries

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz endit_Queen

    call Get_coordinates
    mov al,9
    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz endit_Queen
    
    jmp vertical_for_Queen

endit_Queen:
    ;mov ax,Invalid
    ;mov opponent_flag,invalid
    mov Possible_moves_Size,bx
    pop di

    PopALL
ret
Queen_limitations endp

white_pawn_limitations proc far
    PushALL

    call Get_ScreenPos
    lea si,Possible_moves
    mov bx,0

Check_Forward_Movement:
    sub di,7040

    call Check_In_boundaries 
    cmp Boundaries_flag,Invalid
    jz Check_double_forwrd_move 

    call Check_ally
    cmp Ally_flag,Valid 
    jz Check_double_forwrd_move

    call Check_opponent
    cmp opponent_flag,Valid
    jz Check_double_forwrd_move
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx    

Check_double_forwrd_move:
    sub di,7040

    call Check_In_boundaries 
    cmp Boundaries_flag,Invalid
    jz Check_Right_Movement

    call Check_ally
    cmp Ally_flag,Valid 
    jz Check_Right_Movement

    call Check_opponent
    cmp opponent_flag,Valid
    jz Check_Right_Movement

    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    cmp dx,135
    ja Check_Right_Movement
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

    
Check_Right_Movement:
    add di,7040
    add di,22

    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Check_Left_Movement 

    call Check_ally
    cmp Ally_flag,Valid 
    jz Check_Left_Movement

    call Check_opponent
    cmp opponent_flag,Invalid
    jz Check_Left_Movement
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

Check_Left_Movement:
    sub di,44

    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz No_Left_Kill_Movement 

    call Check_ally
    cmp Ally_flag,Valid 
    jz No_Left_Kill_Movement

    call Check_opponent
    cmp opponent_flag,Invalid
    jz No_Left_Kill_Movement
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

No_Left_Kill_Movement:
    mov Possible_moves_Size,bx
    PopALL
ret
endp white_pawn_limitations

Black_pawn_limitations proc far
    PushALL

    call Get_ScreenPos
    lea si,Possible_moves
    mov ax,0
    mov Possible_moves_Size,ax
    mov bx,0

Check_Forward_Movement2:
    add di,7040

    call Check_In_boundaries 
    cmp Boundaries_flag,Invalid
    jz Check_double_forwrd_move2 

    call Check_ally
    cmp Ally_flag,Valid 
    jz Check_double_forwrd_move2

    call Check_opponent
    cmp opponent_flag,Valid
    jz Check_double_forwrd_move2
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx
    

Check_double_forwrd_move2:
    add di,7040

    call Check_In_boundaries 
    cmp Boundaries_flag,Invalid
    jz Check_Right_Movement2

    call Check_ally
    cmp Ally_flag,Valid 
    jz Check_Right_Movement2

    call Check_opponent
    cmp opponent_flag,Valid
    jz Check_Right_Movement2

    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    cmp dx,25
    jb Check_Right_Movement2
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

    
Check_Right_Movement2:
    sub di,7040
    add di,22

    call Check_In_boundaries                ; Hena el mafrood n3ml tar2eya
    cmp Boundaries_flag,Invalid
    jz Check_Left_Movement2 

    call Check_ally
    cmp Ally_flag,Valid 
    jz Check_Left_Movement2

    call Check_opponent
    cmp opponent_flag,Invalid
    jz Check_Left_Movement2
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

Check_Left_Movement2:
    sub di,44

    call Check_In_boundaries                ; Hena el mafrood n3ml tar2eya
    cmp Boundaries_flag,Invalid
    jz No_Left_Kill_Movement2 

    call Check_ally
    cmp Ally_flag,Valid 
    jz No_Left_Kill_Movement2

    call Check_opponent
    cmp opponent_flag,Invalid
    jz No_Left_Kill_Movement2
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

No_Left_Kill_Movement2:
    mov Possible_moves_Size,bx
    PopALL
ret
endp Black_pawn_limitations

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
    int 10h                             ;Getting the corner point to get the background color

    call Draw_Square                    ;Drawing in the old position with the background color

    PopALL

Drawing:   
    cmp al,0
    je Black_Troop

    cmp bl,16
    jL Pawn_1

    cmp bl,20
    jL Rook_1

    cmp bl,24
    jL Knight_1

    cmp bl,28
    jL Bishop_1

    cmp bl,30
    jL Queen_1

    cmp bl,31
    jL King_1

Black_Troop:

    cmp bl,4
    jL Rook_1

    cmp bl,8
    jL Knight_1

    cmp bl,12
    jL Bishop_1

    cmp bl,14
    jL Queen_1

    cmp bl,16
    jL King_1

Pawn_1:
    Call Draw_Pawn
    jmp FnEnd
Rook_1:
    Call Draw_Rook
    jmp FnEnd
Knight_1:
    Call Draw_Knight
    jmp FnEnd
Bishop_1:
    Call Draw_Bishop
    jmp FnEnd
Queen_1:
    Call Draw_Queen
    jmp FnEnd
King_1:
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
    call Get_ScreenPos
    cmp al,0
    jz Black2_troop
    lea si,white_DI_val
    jmp DummyEnd
Black2_troop:
    lea si,black_DI_val

DummyEnd:
    mov [si + bx],di

ret
ENDP Move  

White_Pieces_Check_Existing proc           ;Choosing piece to move
    ;PushALL 

    lea si,white_x_pos
    lea di,white_y_pos

    mov ax,0                          ;Index of my piece

My_Piece_Checks:
    
    cmp [si],cx
    jne Not_My_Piece

    cmp [di],dx
    jne Not_My_Piece

    jmp My_Piece_Exists

Not_My_Piece:
    add si,2                          ;Checking for my next piece
    add di,2
    add ax,2
    cmp ax,30
    ja S1
    jmp My_Piece_Checks
S1:
    jmp Return

My_Piece_Exists:                      ;Index of piece is in ax   
    mov selected_Troop_Index,ax
    cmp ax,14
    jLe Piece_is_White_Pawn

    cmp ax,18
    ja dummyjmp5
     jmp Piece_is_White_Rook
     dummyjmp5:
    cmp ax,22
     ja dummyjmp4
     jmp Piece_is_White_Knight
     dummyjmp4:

    cmp ax,26
    ja dummyjmp1 
    jmp Piece_is_White_Bishop
dummyjmp1:
    cmp ax,28
    jne dummyjmp2
    jmp Piece_is_White_Queen
dummyjmp2:
    cmp ax,30
    jne dummyjmp3 
    jmp Piece_is_White_King
dummyjmp3:

Piece_is_White_Pawn:   ;/*****************************  Pawn Limitations  *******************************/
    call white_pawn_limitations
    jmp Return

Piece_is_White_Rook:
    call Rook_limitations
    jmp Return

Piece_is_White_Knight:
    call Knight_limitations 
    jmp Return

Piece_is_White_Bishop:
    call Bishop_limitations
    jmp Return

Piece_is_White_Queen:
    ; Only 1 Queen
    call Queen_limitations
    jmp Return

Piece_is_White_King:
    ; Only 1 king
    call king_limitations
    jmp Return

    ;PopALL
Return:

ret
endp White_Pieces_Check_Existing

Black_Pieces_Check_Existing proc far
    ;PushALL
    lea si,black_x_pos
    lea di,black_y_pos

    mov ax,0                          ;Index of my piece

My_Piece_Checks_Black:
    
    cmp [si],cx
    jne Not_My_Piece_Black

    cmp [di],dx
    jne Not_My_Piece_Black

    jmp My_Piece_Exists_Black

Not_My_Piece_Black:
    add si,2                          ;Checking for my next piece
    add di,2
    add ax,2
    cmp ax,30
    jbe My_Piece_Checks_Black
    jmp Return2

My_Piece_Exists_Black:                      ;Index of piece is in ax   

    mov selected_Troop_Index,ax
    cmp ax,2
    jLe Piece_is_Black_Rook

    cmp ax,6
    jLe Piece_is_Black_Knight

    cmp ax,10
    jLe Piece_is_Black_Bishop

    cmp ax,12
    jne A1
    jmp Piece_is_Black_Queen
A1:

    cmp ax,14
    jne B1
    jmp Piece_is_Black_King
B1:

Piece_is_Black_Pawn:   ;/*****************************  Pawn Limitations  *******************************/
    ; Call black pawn here
    call Black_pawn_limitations
    jmp Return2

Piece_is_Black_Rook:
    call Rook_limitations
    jmp Return2

Piece_is_Black_Knight:
    call Knight_limitations
    jmp Return2

Piece_is_Black_Bishop:
    call Bishop_limitations
    jmp Return2

Piece_is_Black_Queen:
    call Queen_limitations
    jmp Return2

Piece_is_Black_King:
    ; Only 1 king
    call king_limitations
    jmp Return2

    ;PopALL
Return2:
ret
endp Black_Pieces_Check_Existing

;********************************************************************
;********************************************************************
;********************************************************************
Valid_Movement proc far
    
    PushALL
    call Get_ScreenPos                ;Gives di of selected troop
    mov ax,di
    mov Previous_Selection,ax

again:    
    mov al,0AH     
    call DrawSquareBorder
    mov ah,1                            ;Waiting for any key to be pressed
    int 16h
    jz again

    mov ah,0
    int 16h

    cmp al,100        ;d
    jz MoveRight2
    
    cmp al,97         ;a
    jz MoveLeft2
    
    cmp al,115        ;s
    jnz S2
    jmp MoveDown2
S2:
    
    cmp al,119        ;w
    jnz W2
    jmp MoveUp2
W2:
    
    cmp al,113        ;Q
    jnz F2
    jmp empty_there
F2:
    jmp again

MoveRight2:
    
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border
    
    mov al,0AH
    add cx,22     ;22
    cmp cx,229    ;229
    ja RotateRight2
    mov al,0AH
    call DrawSquareBorder

    jmp again

MoveLeft2:
    
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border

    mov al,0AH
    sub cx,22
    cmp cx,75
    jb RotateLeft2
    mov al,0AH
    Call DrawSquareBorder

    jmp again

MoveDown2:
    
    mov ah,0DH
    mov bh,0
    int 10h
   
    call DrawSquareBorder                ;Deleting previous border

    mov al,0AH
    add dx,22
    cmp dx,157
    ja RotateDown2
    mov al,0AH
    Call DrawSquareBorder

    jmp again

MoveUp2:
    
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border

    mov al,0AH
    sub dx,22
    cmp dx,3
    jb RotateUp2 

    jmp again

RotateRight2:
    mov cx,75
    jmp again
RotateLeft2:
    mov cx,229
    jmp again
RotateDown2:
    mov dx,3
    jmp again
RotateUp2:
    mov dx,157
    jmp again
    
empty_there:
    call Get_ScreenPos         ;Gives di of destination

    cmp di,Previous_Selection
    jnz Dummy
    jmp Return_To_Select

Dummy:    
    mov ax,Black_Pieces
    lea si,Player_Pieces_Color
    cmp ax,[si]
    jz white_opp
    lea si,black_DI_val
    jmp Dummyjmp6

white_opp:
    lea si,white_DI_val

Dummyjmp6:
    ;movecursor 0A0Ah
    ;DisplayString Possible_moves

    lea si,Possible_moves
    mov ax,Possible_moves_Size

Loop_Possible_moves:

    cmp [si],di
    je Dumm

    add si,2
    dec ax      ;Counter for possible moves array
    cmp ax,0
    ja Loop_Possible_moves

    jmp again

Dumm:
    call Check_opponent
    cmp opponent_flag,Valid
    jz opponent_there

    ;call Check_ally   
    ;cmp Ally_flag,Valid
    ;jz Ally_there

    jmp Start2

;Ally_there:
        ;get_coordinates
        ;draw ally henak

opponent_there:
    
    array_loop:

        cmp bx,[si]
        jz capture
        ;loop possible moves array
        ;law el index gwah yeb2a opponent
        ;get_coordinates
        ;draw ally henak
        add si,2
        dec Possible_moves_Size
        jnz array_loop
        jmp Start2
    capture:
        mov al,0FH                                  ;draw opponent on the side of the chessboard
        mov bx,Opponent_Troop_Index
        mov cx,273
        mov dx,3
        Call Move
    
Start2:
    call Get_coordinates
    ;selected piece draw

    mov ax,Black_Pieces       ;get pixel color
    lea si,Player_Pieces_Color
    cmp [si],ax
    jz Black_Player

    mov al,0FH
    jmp White_Player

Black_Player:
    mov al,0H

White_Player:
    mov bx,selected_Troop_Index
    call Move

Return_To_Select:
    call Delete_selection
    PopALL
ret
Valid_Movement endp    
;********************************************************************
;********************************************************************
;********************************************************************
User_Move Proc far

    mov ax,Black_Pieces
    lea si,Player_Pieces_Color
    cmp [si],ax
    jz Black_Pieces_Checked
    mov cx,75                           ;First piece of the white side
    mov dx,157
    mov al,0AH

    jmp CheckChar

Black_Pieces_Checked:
    mov cx,75                           ;First piece of the black side
    mov dx,3
    mov al,0AH

CheckChar:
    mov al,0AH
    call DrawSquareBorder
    mov ah,1                            ;Waiting for any key to be pressed
    int 16h
    jz CheckChar

    mov ah,0
    int 16h
;Check Possible_moves_size
;if 0 continue this code
; if not 0 call function Valid_Movement
    cmp Possible_moves_size,0
    jz GOO
    call Valid_Movement
GOO:
    cmp al,100        ;d
    jz MoveRight
    
    cmp al,97         ;a
    jz MoveLeft
    
    cmp al,115        ;s
    jnz S
    jmp MoveDown
S:
    
    cmp al,119        ;w
    jnz W
    jmp MoveUp
W:
    
    cmp al,113        ;Q
    jnz F
    jmp Check_Selected_Piece
F:
    jmp CheckChar

MoveRight:
    
    mov ah,0DH
    mov bh,0
    int 10h
   
    call DrawSquareBorder                ;Deleting previous border
    
    mov al,0AH
    add cx,22     ;22
    cmp cx,229    ;229
    ja RotateRight
    mov al,0AH
    call DrawSquareBorder

    jmp CheckChar

MoveLeft:
    
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border

    mov al,0AH
    sub cx,22
    cmp cx,75
    jb RotateLeft
    mov al,0AH
    Call DrawSquareBorder

    jmp CheckChar

MoveDown:
    
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border

    mov al,0AH
    add dx,22
    cmp dx,157
    ja RotateDown
    mov al,0AH
    Call DrawSquareBorder

    jmp CheckChar

MoveUp:
    
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border

    mov al,0AH
    sub dx,22
    cmp dx,3
    jb RotateUp
    
    jmp CheckChar

Check_Selected_Piece:
    mov ax,Black_Pieces
    lea si,Player_Pieces_Color
    cmp [si],ax
    jz Black_Pieces_Checked2

    call White_Pieces_Check_Existing
    jmp CheckChar

Black_Pieces_Checked2:
    call Black_Pieces_Check_Existing
    jmp CheckChar

RotateRight:
    mov cx,75
    jmp CheckChar
RotateLeft:
    mov cx,229
    jmp CheckChar
RotateDown:
    mov dx,3
    jmp CheckChar
RotateUp:
    mov dx,157
    jmp CheckChar

ENDP User_Move

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
         mov dx,176
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

         get_another_order:
         mov ah,0
         int 16h

         cmp ah,3DH                                 ; if F3 is pressed
         jz chat
         
         cmp ah,3CH                                 ; if F2 is pressed
         jz game

         cmp al,27                                  ; if ESC is pressed
         jnz Dum
        jmp ProgramEnd
    Dum:
         jmp get_another_order         

        chat:
            movecursor 1704h
            DisplayString Waiting_Message
            jmp MainEnd
        game:
            
            call init_board
            mov ax,Black_Pieces
            mov Player_Pieces_Color,ax
                        
            mov bx,20h                        ;Testing the drawing
            call SetDelayTime 
            
            mov al,0FH
            mov bx,16
            mov cx,141
            mov dx,69
            Call Move

            mov bx,20h                        ;Testing the drawing
            call SetDelayTime

            mov al,00H
            mov bx,0
            mov cx,141
            mov dx,113
            Call Move

            mov al,00H
            mov bx,12
            mov cx,185
            mov dx,69
            Call Move

            mov ah,0ch                     ;Clearing keyboard buffer
            int 21h

            PushALL
            call User_Move
            PopALL

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
