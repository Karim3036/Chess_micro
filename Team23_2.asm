

;This file is for the 2 players game on one laptop (Phase 1) but it currently not fully functional


.Model Compact
.Stack 64
.Data

    default_check   dw ?

    Dollar_Sign        equ '$'
    Questionmark_ascii equ '?'

    Valid                   EQU 1
    Invalid                 EQU 0

    enter_name             db 'Please Enter your name:','$'
    Player_name            db  15,?,15 DUP('$') 
    cont                   db 'Press any key to continue','$'
    main_menu_chat         db 'To start chatting press F3','$'
    main_menu_game         db 'To start the game press F2','$'
    main_menu_exit         db 'To end the program press ESC','$'
    Error_Message          db 'Error! Name must start with a letter','$'
    Waiting_Message        db 'Invitation sent, wait for response','$'
    Program_Terminated     db 'Program_Terminated','$'
    Check_King             db 'Check','$'

    Player_1_Wins   db 'Player 1 Wins','$'
    Player_2_Wins   db 'Player 2 Wins','$'

    W_Player_Pos    dw 50315
    B_Player_pos    dw 1035

    side_limit                  dw 4 dup(?)
    Possible_move_boarder       dw 4 dup(?)

    Possible_W_moves_Size         dw 0
    Possible_W_moves              dw 21 dup(?),'$'    ;Max possible moves (for Queen) = Queen

    Possible_B_moves_Size         dw 0
    Possible_B_moves              dw 21 dup(?),'$'    ;Max possible moves (for Queen) = Queen

    Deleted_Selection_Size        dw ?

    Capture_Opponent_array      dw 21 dup(?),'$'        ;********************************************************************************************

    Capture_White_X_Pos   dw 6
    Capture_White_Y_Pos   dw 3

    Capture_Black_X_Pos   dw 254
    Capture_Black_Y_Pos   dw 157

    Opponent_W_Troop_Index  dw ?
    selected_W_Troop_Index  dw ?

    Opponent_B_Troop_Index  dw ?
    selected_B_Troop_Index  dw ?

    Previous_W_Selection    dw ?
    Previous_B_Selection    dw ?

    Destination_W_DI        dw ?
    Destination_B_DI        dw ?   

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

    Player1_Pieces_Color dw ?
    Player2_Pieces_Color dw ?
    White_Pieces        EQU 0
    Black_Pieces        EQU 1

    White_Pieces_Border_Color  EQU 4
    Black_Pieces_Border_Color  EQU 0AH

    White_Possible_Moves_Border   EQU 6
    Black_Possible_Moves_Border   EQU 9

    White_Selected_Border_Color   EQU 0EH
    Black_Selected_Border_Color   EQU 1

    White_Player_Moving           EQU 1
    Black_Player_Moving           EQU 0

    Current_Move dw ?
    
    Moves_Border_Color db ?

    black_x_pos  dw 75,229,97,207,119,185,141,163,75,97,119,141,163,185,207,229 
    black_y_pos  dw 8 dup(3),8 dup(25)
    black_DI_val dw 1035,1189,1057,1167,1079,1145,1101,1123,8075,8097,8119,8141,8163,8185,8207,8229

    Black_King_Index    EQU 14
    White_King_Index    EQU 30

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
    mov bh,0
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
    PushALL
    mov ax,0600h
    mov bh,0
    mov cx,0
    mov dx,184fh
    int 10h
    PopALL
ENDM ClearScreen

RotateRight MACRO
    mov cx,75
endm RotateRight

RotateLeft MACRO
    mov cx,229
endm RotateLeft
RotateDown MACRO
    mov dx,3
endm RotateDown
RotateUp MACRO
    mov dx,157
endm RotateUp

Delete_Previous_Border MACRO
    mov ah,0DH
    mov bh,0
    int 10h
    
    call DrawSquareBorder                ;Deleting previous border

ENDM Delete_Previous_Border    

clear_status_bar proc
PushALL             
    mov dx,177          ;start of status bar
status_bar_cols:
    mov cx,100   
    mov al,0            ;black color
    mov ah,0ch
status_bar_rows: int 10h
    inc cx
    cmp cx,320
    jnz status_bar_rows
    inc dx
    cmp dx,201
    jb status_bar_cols
PopALL
ret
clear_status_bar endp

SetDelayTime Proc near                            ;delay 50 ms                  
    Timer2:                                  
        mov cx,0C350H                             
        D1: Loop D1
        dec bx
        jnz Timer2
ret
SetDelayTime ENDP 		

;.......................................
;               DEFAULT                ;
;.......................................

TO_DEFAULT proc far

;********************************************************************************************************;
;*************** X positions and Y positions array are set in the init board function *******************;
;********************************************************************************************************;

    PushALL

    mov ax,default_check
    lea si,side_limit
    lea bp,Possible_move_boarder
    mov cx,4
loop1:
        mov [si],ax                              ;    side_limit                  dw 4 dup(?)                           
        mov [bp],ax                              ;    Possible_move_boarder       dw 4 dup(?)
        add si,2            
        add bp,2
    loop loop1              

    mov bx,0
    mov Possible_W_moves_Size,bx                   ;    Possible_moves_Size         dw 0

    lea si,Possible_W_moves                        ;    Possible_moves              dw 21 dup(?),'$
    lea bp,Capture_Opponent_array
    mov cx,21                                    ;    Capture_Opponent_array      dw 21 dup(?)   
loop2:
        mov [si],ax                                            
        mov [bp],ax                 
        add si,2            
        add bp,2
    loop loop2

    mov bx,0
    mov Possible_B_moves_Size,bx                   ;    Possible_moves_Size         dw 0

    lea si,Possible_B_moves                        ;    Possible_moves              dw 21 dup(?),'$
    lea bp,Capture_Opponent_array
    mov cx,21                                    ;    Capture_Opponent_array      dw 21 dup(?)   
loop3:
        mov [si],ax                                            
        mov [bp],ax                 
        add si,2            
        add bp,2
    loop loop2   

    mov ax,6
    mov bx,3
    mov cx,254                                  ;    Capture_White_X_Pos   dw 6
    mov dx,157                                  ;    Capture_White_Y_Pos   dw 3
    mov Capture_White_X_Pos,ax                  ;
    mov Capture_White_Y_Pos,bx                  ;    Capture_Black_X_Pos   dw 254
    mov Capture_Black_X_Pos,cx                  ;    Capture_Black_Y_Pos   dw 157
    mov Capture_Black_Y_Pos,dx

    mov ax, default_check                       ;    Opponent_Troop_Index  dw ?
    mov Opponent_W_Troop_Index,ax                 ;    selected_Troop_Index  dw ?
    mov Opponent_B_Troop_Index,ax                 ;    selected_Troop_Index  dw ?
    mov selected_W_Troop_Index,ax                 ;
    mov selected_B_Troop_Index,ax                 ;               
    mov Previous_W_Selection,ax                   ;    Previous_Selection    dw ?
    mov Previous_B_Selection,ax                   ;    Previous_Selection    dw ?
    mov Destination_W_DI,ax                       ;    Destination_DI        dw ?  
    mov Destination_B_DI,ax                       ;    Destination_DI        dw ?            
    
    mov bx,Valid    
    mov dx,Invalid
    mov Boundaries_flag,bx                      ;    Boundaries_flag       dw Valid  
    mov Ally_flag,bx                            ;    Ally_flag             dw Valid
    mov opponent_flag,dx                        ;    opponent_flag         dw Invalid

;********************************************************;
;*******************  White DI Array ********************;
;********************************************************;
lea si,white_DI_val
mov ax,43275
mov bx,43297
mov cx,43319
mov dx,43341
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2
mov ax,43363
mov bx,43385
mov cx,43407
mov dx,43429
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2

mov ax,50315
mov bx,50469
mov cx,50337
mov dx,50447
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2
mov ax,50359
mov bx,50425
mov cx,50381
mov dx,50403
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2

;*********************************************************;
;*******************  Black DI Array  ********************;
;*********************************************************;
lea si,black_DI_val
mov ax,1035
mov bx,1189
mov cx,1057
mov dx,1167
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2
mov ax,1079
mov bx,1145
mov cx,1101
mov dx,1123
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2
mov ax,8075
mov bx,8097
mov cx,8119
mov dx,8141
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2
mov ax,8163
mov bx,8185
mov cx,8207
mov dx,8229
mov [si],ax
add si,2
mov [si],bx
add si,2
mov [si],cx
add si,2
mov [si],dx
add si,2

lea si,Player_name               ;Player_name  db  15,?,15 DUP('$')
mov ax,15
mov [si],ax
inc si
mov ax,default_check
mov [si],ax
inc si

mov ax,Dollar_Sign
mov cx,15
Loop_Name:
    mov [si],ax
    inc si
    Loop Loop_Name

xor ax,ax
xor bx,bx
xor cx,cx
xor dx,dx

PopALL

ret
TO_DEFAULT endp

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
	mov cx,75
	mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
	mov al,0
    call Draw_Rook
    mov cx,229
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Rook
    mov cx,97
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Knight
    mov cx,207
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Knight
    mov cx,119
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Bishop
    mov cx,185
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Bishop
    mov cx,141
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Queen
    mov cx,163
    mov dx,3
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_King
    mov cx,75
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,97
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,119
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,141
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,163
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,185
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,207
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,229
    mov dx,25
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn

	lea si,white_x_pos
    lea di,white_y_pos

	mov cx,75
	mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
	mov al, 0fh
    call Draw_Pawn
    mov cx,97
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,119
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,141
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,163
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,185
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,207
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,229
    mov dx,135
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Pawn
    mov cx,75
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Rook
    mov cx,229
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Rook
    mov cx,97
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Knight
    mov cx,207
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Knight
    mov cx,119
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Bishop
    mov cx,185
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Bishop
    mov cx,141
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
    call Draw_Queen
    mov cx,163
    mov dx,157
    mov [si],cx
    mov [di],dx
    add si,2
    add di,2
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
                          ;bp = Selected position
    PushALL
    mov ax,Invalid
    mov opponent_flag,ax

    cmp bp,B_Player_pos
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

    cmp bp,B_Player_pos
    jz BBBBB
    mov Opponent_B_Troop_Index,bx
    jmp FnEnd3

BBBBB:
    mov Opponent_W_Troop_Index,bx 

FnEnd3:
    PopALL
ret
Check_opponent endp



Check_ally proc far      ;see if there is a friendly troop
                          ;di = next position
                          ;bp = selected position
    PushALL
    cmp bp,B_Player_pos
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

    cmp Current_Move,White_Player_Moving
    jz Delete_White

    lea si,Possible_B_moves
    mov bx,Possible_B_moves_Size
    mov Deleted_Selection_Size,bx
    jmp Delete_next

Delete_White:    
    LEA SI,Possible_W_moves
    mov bx,Possible_W_moves_Size
    mov Deleted_Selection_Size,bx

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
    dec Deleted_Selection_Size
    jnz Delete_next

    PopALL
ret
Delete_selection endp

king_limitations proc far 

    PushALL
    call Get_ScreenPos
    mov bp,di   ;Saves Original di in bp
    cmp bp,B_Player_pos     ; Player Position is updated in user move function, with each character selected
    jz B_Player2

    lea si,Possible_W_moves
    jmp ResumeKing

B_Player2:
    lea SI,Possible_B_moves       ;mov di to the array if valid move

ResumeKing:
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
 
;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border
    
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
    
;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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
    
;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border
    
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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next8:
    cmp bp,B_Player_pos
    jz B_Border_K

    lea si,Possible_W_moves
    mov Possible_W_moves_Size,bx
    mov al,White_Possible_Moves_Border
    mov Moves_Border_Color,al
    jmp Draw_Possible_Moves_Borders_K

B_Border_K:
    lea si,Possible_B_moves
    mov Possible_B_moves_Size,bx
    mov al,Black_Possible_Moves_Border
    mov Moves_Border_Color,al

Draw_Possible_Moves_Borders_K:
    mov di,[si]
    call Get_coordinates
    mov al,Moves_Border_Color
    call Draw_possiblemove_Border

    add si,2
    dec bx
    jnz Draw_Possible_Moves_Borders_K
    
    PopALL
    ret
king_limitations endp

Rook_limitations proc far
    PushALL

    call Get_ScreenPos       ;Returns di
    mov bp,di   ;Saves Original di in bp
    cmp bp,B_Player_pos     ; Player Position is updated in user move function, with each character selected
    jz B_Player

    lea si,Possible_W_moves
    jmp ResumeRook

B_Player:
    lea SI,Possible_B_moves       ;mov di to the array if valid move

ResumeRook:
    mov bx,0    ;Possible moves array size
    
    push di

horizontal_right:
    add di,22

    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz Get_di          ; Position out of right boundaries 

    call Check_ally
    cmp Ally_flag,Valid         ;Ally found
    jz Get_di

    ;call Get_coordinates    ;Draws border in the possible move
    ;mov al,9
    ;call Draw_possiblemove_Border

    mov [SI],di            ;Possible moves array
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di
    
    jmp horizontal_right

Get_di:    
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

;    call Get_coordinates
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di2

    jmp horizontal_left

Get_di2:

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

;    call Get_coordinates
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di3
    
    jmp vertical_back

Get_di3:
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

;    call Get_coordinates
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz endit

    jmp vertical_for

endit:
    cmp bp,B_Player_pos
    jz B_Border_R

    lea si,Possible_W_moves
    mov al,White_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_W_moves_Size,bx

    jmp Draw_Possible_Moves_Borders_R

B_Border_R:
    lea si,Possible_B_moves
    mov al,Black_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_B_moves_Size,bx

Draw_Possible_Moves_Borders_R:
    mov di,[si]
    call Get_coordinates
    mov al,Moves_Border_Color
    call Draw_possiblemove_Border
    add si,2
    dec bx
    jnz Draw_Possible_Moves_Borders_R

    pop di
    PopALL
    ret
Rook_limitations endp

Knight_limitations proc far
PushALL

    call Get_ScreenPos
    mov bp,di   ;Saves Original di in bp
    cmp bp,B_Player_pos     ; Player Position is updated in user move function, with each character selected
    jz B_Player4

    lea si,Possible_W_moves
    jmp ResumeKnight

B_Player4:
    lea SI,Possible_B_moves       ;mov di to the array if valid move

ResumeKnight:
    mov bx,0
    
    push di
    
    add di,7084
    call Check_In_boundaries
    cmp Boundaries_flag,Invalid
    jz next1_knight

    call Check_ally
    cmp Ally_flag,Valid 
    jz next1_knight

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
next8_knight:
    cmp bp,B_Player_pos
    jz B_Border_Kn

    lea si,Possible_W_moves
    mov al,White_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_W_moves_Size,bx

    jmp Draw_Possible_Moves_Borders_Kn

B_Border_Kn:
    lea si,Possible_B_moves
    mov al,Black_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_B_moves_Size,bx

Draw_Possible_Moves_Borders_Kn:
    mov di,[si]
    call Get_coordinates
    mov al,Moves_Border_Color
    call Draw_possiblemove_Border
    add si,2
    dec bx
    jnz Draw_Possible_Moves_Borders_Kn

    pop di
    PopALL
ret
Knight_limitations endp

Bishop_limitations proc far
PushALL

    call Get_ScreenPos

    mov bp,di   ;Saves Original di in bp
    cmp bp,B_Player_pos     ; Player Position is updated in user move function, with each character selected
    jz B_Player_B

    lea si,Possible_W_moves
    jmp ResumeBishop

B_Player_B:
    lea SI,Possible_B_moves       ;mov di to the array if valid move

ResumeBishop:

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di1_bishop

    jmp Top_right

Get_di1_bishop:
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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di2_bishop

    jmp Top_left

Get_di2_bishop:
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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz Get_di3_bishop

    jmp Bottom_right

Get_di3_bishop:
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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx

    call Check_opponent
    cmp opponent_flag,Valid
    jz end_it_bishop

    jmp Bottom_left

end_it_bishop:
    cmp bp,B_Player_pos
    jz B_Border_B

    lea si,Possible_W_moves
    mov al,White_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_W_moves_Size,bx

    jmp Draw_Possible_Moves_Borders_B

B_Border_B:
    lea si,Possible_B_moves
    mov al,Black_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_B_moves_Size,bx

Draw_Possible_Moves_Borders_B:
    mov di,[si]
    call Get_coordinates
    mov al,Moves_Border_Color
    call Draw_possiblemove_Border
    add si,2
    dec bx
    jnz Draw_Possible_Moves_Borders_B

    pop di

PopALL
ret
Bishop_limitations endp

Queen_limitations proc far
PushALL
    ;same as bishop
    call Get_ScreenPos
    mov bp,di   ;Saves Original di in bp
    cmp bp,B_Player_pos     ; Player Position is updated in user move function, with each character selected
    jz B_Player_Q

    lea si,Possible_W_moves
    jmp ResumeQueen

B_Player_Q:
    lea SI,Possible_B_moves       ;mov di to the array if valid move

ResumeQueen:
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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates    ;Draws border in the possible move
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates
;    mov al,9
;    call Draw_possiblemove_Border

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

;    call Get_coordinates
;    mov al,9
;    call Draw_possiblemove_Border

    mov [SI],di
    add si,2
    inc bx
    
    call Check_opponent
    cmp opponent_flag,Valid
    jz endit_Queen
    
    jmp vertical_for_Queen

endit_Queen:
    cmp bp,B_Player_pos
    jz B_Border_Q

    lea si,Possible_W_moves
    mov al,White_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_W_moves_Size,bx

    jmp Draw_Possible_Moves_Borders_Q

B_Border_Q:
    lea si,Possible_B_moves
    mov al,Black_Possible_Moves_Border
    mov Moves_Border_Color,al
    mov Possible_B_moves_Size,bx

Draw_Possible_Moves_Borders_Q:
    mov di,[si]
    call Get_coordinates
    mov al,Moves_Border_Color
    call Draw_possiblemove_Border
    add si,2
    dec bx
    jnz Draw_Possible_Moves_Borders_Q

    pop di
    PopALL
ret
Queen_limitations endp

white_pawn_limitations proc far
    PushALL

    mov bp,W_Player_Pos
    mov di,W_Player_Pos
    lea si,Possible_W_moves
    ;mov ax,0
    ;mov Possible_W_moves_Size,ax
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
    mov al,White_Possible_Moves_Border
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
    cmp dx,91               ;Checks double jump in first move
    jb Check_Right_Movement
    mov al,White_Possible_Moves_Border
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
    mov al,White_Possible_Moves_Border
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
    mov al,White_Possible_Moves_Border
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

No_Left_Kill_Movement:
    mov Possible_W_moves_Size,bx
    PopALL
ret
white_pawn_limitations endp 

Black_pawn_limitations proc far
    PushALL

    mov bp,B_Player_pos  ; Keeps Initial Position of Pawn
    mov di,B_Player_pos  ; Initial Position of Pawn
    ;call Get_ScreenPos
    ;mov Possible_B_moves_Size,ax
    lea si,Possible_B_moves
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
    mov al,Black_Possible_Moves_Border
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
    cmp dx,69
    ja Check_Right_Movement2
    mov al,Black_Possible_Moves_Border
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
    mov al,Black_Possible_Moves_Border
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
    mov al,Black_Possible_Moves_Border
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

No_Left_Kill_Movement2:
    mov Possible_B_moves_Size,bx
    PopALL
ret
Black_pawn_limitations endp 

Move proc far    
;************** Piece new position is changed in array inside the function, array should enter the function holding the previous position ***************;

    ;al contains the piece color
    ;bx contains troop index
    ;cx contains x position
    ;dx contains y position

    PushALL
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
    PopALL
ret
Move ENDP 

White_Pieces_Check_Existing proc           ;Choosing piece to move
    ;PushALL 
    mov di,W_Player_Pos                    ;To make sure that the cx and dx contains the pos of the white player
    call Get_coordinates

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
    PushALL
    call Get_ScreenPos                ;Gives di of selected troop
    mov al,1
    call Draw_possiblemove_Border     ; Draws Border on selected piece
    mov Previous_W_Selection,di       ; Saves di of Selected troop
    PopALL

    mov selected_W_Troop_Index,ax
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
    ;cmp ax,30
    ;jne Piece_is_White_Pawn 
    jmp Piece_is_White_King

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
White_Pieces_Check_Existing endp

Black_Pieces_Check_Existing proc far
    ;PushALL
    lea si,black_DI_val

    mov ax,0                          ;Index of my piece
    mov bx,B_Player_pos

My_Piece_Checks_Black:
    
    cmp [si],bx
    jne Not_My_Piece_Black

    jmp My_Piece_Exists_Black

Not_My_Piece_Black:
    add si,2                          ;Checking for my next piece
    ;add di,2
    add ax,2
    cmp ax,30
    jbe My_Piece_Checks_Black
    jmp Return2     ; Returns from function with no piece selected

My_Piece_Exists_Black:                      ;Index of piece is in ax
    PushALL
    mov di,B_Player_pos
    call Get_coordinates                ;Gives di of selected troop
    mov al,Black_Selected_Border_Color
    call Draw_possiblemove_Border    ; Draws a border around the selected piece

    mov Previous_B_Selection,di   ; Saves the selected troop di
    PopALL

    mov selected_B_Troop_Index,ax

    cmp ax,2
    jLe Piece_is_Black_Rook

    cmp ax,6
    jLe Piece_is_Black_Knight

    cmp ax,10
    jLe Piece_is_Black_Bishop

    cmp ax,12
    je Piece_is_Black_Queen

    cmp ax,14
    je Piece_is_Black_King

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
Black_Pieces_Check_Existing endp 

;********************************************************************;
;                  Check if the movement made is valid               ;
;********************************************************************;


Valid_Movement proc far     ; di containing the di value of destination   
    
    ;call Get_ScreenPos                ;Gives di of selected troop

;    PushALL
;    cmp Current_Move,White_Player_Moving
;    jz White_King
;
;    mov bx,Black_King_Index     ; Index of Black King
;    lea si,black_DI_val
;    mov ax,[si + bx]     ;King's di value
;    lea bp,Possible_W_moves
;    mov cx,Possible_W_moves_Size   ;Counter for possible moves arrays
;
;    jmp Check_King_Loop
;
;White_King:
;    mov bx,White_King_Index  ; Index of White King
;    lea si,white_DI_val
;    add si,bx
;    mov ax,[si]     ;King's di value
;    lea si,Possible_B_moves
;    mov cx,Possible_B_moves_Size   ;Counter for possible moves arrays
;
;Check_King_Loop:     ;Checks If the king can be killed (appears in possible moves)
;    cmp ax,[si]
;    jz King_Found
;
;    add si,2
;    Loop Check_King_Loop
;
;    jmp No_King
;
;King_Found:
;    call clear_status_bar
;
;    movecursor 1712h
;    DisplayString Check_King
;    jmp Dummyjmp12
;
;No_King:
;    call clear_status_bar
;    
;Dummyjmp12:
;    PopALL

;*****************************  End of King Check  **********************************;

;*******************  di,cx,dx contains Destination position  ***********************;
   
;empty_there:

;**********************************************************************************************************************************************************;
;**********************************************************************************************************************************************************;
;**********************************************************************************************************************************************************;
    PushALL

    call Get_ScreenPos         ;Gives di of destination

    cmp Current_Move,White_Player_Moving
    jz White_Movement

    cmp di,Previous_B_Selection
    jnz DummyJump
    jmp Return_To_Select

DummyJump:
    mov Destination_B_DI,di      ;Saves the destination position
    lea si,Possible_B_moves
    mov ax,0
    mov bx,Possible_B_moves_Size
    jmp Loop_Possible_moves

White_Movement:
    cmp di,Previous_W_Selection
    jnz Dummyjmp6
    jmp Return_To_Select

Dummyjmp6:
    mov Destination_W_DI,di      ;Saves the destination position
    lea si,Possible_W_moves
    mov ax,0
    mov bx,Possible_W_moves_Size

Loop_Possible_moves:     ; Checks if the Q pressed is a possible move

    cmp [si],di
    je Dumm

    add si,2
    inc ax      ;Counter for possible moves array
    cmp ax,bx
    jbe Loop_Possible_moves

    jmp Return_To_Select

Dumm:    ;********************************** Troop will move *************************************;
    PushALL
    cmp Current_Move,White_Player_Moving
    jz Move_White

    mov di,Previous_B_Selection
    jmp DummyJump2

Move_White:
    mov di,Previous_W_Selection                ; Deletes Previous selection border

DummyJump2:    
    call Get_coordinates

    mov ah,0DH
    mov bh,0
    int 10h

    call Draw_possiblemove_Border

    PopALL

    cmp Current_Move,Black_Player_Moving
    jz white_opp

    lea si,black_DI_val
    jmp Dummy

white_opp:
    lea si,white_DI_val

Dummy:
    call Check_opponent
    cmp opponent_flag,Valid
    jz opponent_there

    jmp Start2

opponent_there:
    cmp Current_Move,White_Player_Moving
    jz Kill_Black

    mov ax,Possible_B_moves_Size
    mov bx,Opponent_W_Troop_Index    ;Giving bx the index of the captured array

    jmp Kill_White

Kill_Black:
    mov ax,Possible_W_moves_Size
    mov bx,Opponent_B_Troop_Index    ;Giving bx the index of the captured array

Kill_White:
    push ax
    ;mov ax,0
    array_loop:

        cmp di,[si]
        jz capture
        ;loop possible moves array
        ;law el index gwah yeb2a opponent
        ;get_coordinates
        ;draw ally henak
        add si,2
        ;add ax,2
        dec ax
        jnz array_loop
        
    capture:
        pop ax  ; ax contains Possible moves size

        push di

        cmp Current_Move,Black_Player_Moving
        jz Black_Color

        cmp Opponent_B_Troop_Index,Black_King_Index
        jnz Dummm

        ClearScreen
        movecursor 0104h
        DisplayString Player_1_Wins       ; White wins
        mov bx,40h                        ;Testing the drawing
        call SetDelayTime

        jmp far ptr Game_Over     ;Returns to MainScreen

    Dummm:
        mov al,00H
        mov cx,Capture_Black_X_Pos
        mov dx,Capture_Black_Y_Pos
        ;add Capture_White_X_Pos,20
        sub Capture_Black_Y_Pos,18
        cmp Capture_Black_Y_Pos,129
        jna Du
        mov Capture_Black_X_Pos,272
        mov Capture_Black_Y_Pos,157

    Du:
        jmp White_Color

    Black_Color:

        cmp Opponent_W_Troop_Index,White_King_Index
        jnz Dummm2

        ClearScreen
        movecursor 0104h
        DisplayString Player_2_Wins       ;Black Wins
        mov bx,40h                        ;Testing the drawing
        call SetDelayTime

        jmp far ptr Game_Over     ;Returns to MainScreen

    Dummm2:
        mov al,0FH
        mov cx,Capture_White_X_Pos
        mov dx,Capture_White_Y_Pos
        ;add Capture_Black_X_Pos,20
        add Capture_White_Y_Pos,18
        cmp Capture_White_Y_Pos,129
        jna White_Color
        mov Capture_White_X_Pos,24
        mov Capture_White_Y_Pos,3

    White_Color:
        Call Move   ;Draw the captured troop outside the grid
        pop di

Start2:
    call Get_coordinates      ;selected piece draw

    cmp Current_Move,Black_Player_Moving
    jz Black_Player

    mov al,0FH
    mov bx,selected_W_Troop_Index
    jmp Draw_New

Black_Player:
    mov al,0H
    mov bx,selected_B_Troop_Index

Draw_New:   
    call Move

Return_To_Select:
    PushALL
    cmp Current_Move,White_Player_Moving
    jz Clear_White_Selection_Border

    mov di,Previous_B_Selection
    jmp Clear

Clear_White_Selection_Border:
    mov di,Previous_W_Selection                ; Deletes Previous selection border

Clear:    
    call Get_coordinates

    mov ah,0DH
    mov bh,0
    int 10h

    call Draw_possiblemove_Border
    PopALL

    call Delete_selection
    PopALL
ret
Valid_Movement endp  

;************************************************************************;
;                       Check User's first move                          ;
;************************************************************************;
User_Move Proc far

    mov cx,75                           ;First piece of the white side
    mov dx,157
    mov al,4

    call DrawSquareBorder   ;To Draw the border with every move

    call Get_ScreenPos
    mov W_Player_Pos,di     ;Saves the start pos of the White Player in a variable

    mov cx,75                           ;First piece of the black side
    mov dx,3
    mov al,0AH

    call DrawSquareBorder   ;To Draw the border with every move

    call Get_ScreenPos      
    mov B_Player_pos,di     ;Saves the start pos of the Black Player in a variable

CheckChar:
;Check Possible_moves_size
;if 0 continue this code
; if not 0 call function Valid_Movement
    cmp Possible_W_moves_Size,0
    jz No_W_Selection

    mov ah,1                            ;Waiting for any key to be pressed
    int 16h
    jz No_W_Selection

    mov ah,0
    int 16h

    cmp ah,26H                  ;L
    jnz Check_New_Press

    mov ax,White_Player_Moving
    mov Current_Move,ax        ; To be checked inside the valid movement function

    call Valid_Movement        ; Have di of destination
    mov W_Player_Pos,di        ;To start the next loop from the destination position
    jmp CheckChar

No_W_Selection:
    cmp Possible_B_moves_Size,0
    jz No_B_Selection

    mov ah,1                            ;Waiting for any key to be pressed
    int 16h
    jz No_B_Selection

    mov ah,0
    int 16h

    cmp ah,16                 ;Q
    jnz Check_New_Press

    mov ax,Black_Player_Moving
    mov Current_Move,ax

    call Valid_Movement
    mov B_Player_pos,di        ;To start the next loop from the destination position
    jmp CheckChar

No_B_Selection:
    mov ah,1                            ;Check for any key to pressed
    int 16h
    jnz XX
    jmp CheckChar
XX:

    mov ah,0                            ; Waiting for a key to be pressed
    int 16h
;Check Possible_moves_size
;if 0 continue this code
; if not 0 call function Valid_Movement
    ;cmp Possible_moves_size,0
    ;jz GOO
    ;call Valid_Movement
    ;jmp CheckChar
Check_New_Press:
    cmp ah,32         ;Pressed d
    jnz Z
    jmp MoveRight

Z:
    cmp ah,30         ;Pressed a
    jnz M
    jmp MoveLeft

M:    
    cmp ah,31        ;Pressed s
    jnz S
    jmp MoveDown
S:
    
    cmp ah,17        ;Pressed w
    jnz W
    jmp MoveUp
W:
    
    cmp ah,16        ;Pressed Q
    jnz Player_2_Chars
    jmp Check_B_Selected_Piece

Player_2_Chars:
    cmp ah,4DH        ;Pressed Right Arrow
    jnz Dummy_Player2
    jmp Move_Right_2

Dummy_Player2:
    cmp ah,4BH        ;Pressed Left Arrow
    jnz Dummy_Player21
    jmp Move_Left_2

Dummy_Player21:    
    cmp ah,48H        ;Pressed Up Arrow
    jnz Dummy_Player22
    jmp Move_Up_2

Dummy_Player22:
    cmp ah,50H        ;Pressed Down Arrow
    jnz Dummy_Player23
    jmp Move_Down_2

Dummy_Player23:
    cmp ah,26H         ;Pressed L (To move)
    jnz F
    jmp Check_W_Selected_Piece

F:
    cmp ah,3EH       ;Pressed F4   Terminates Program
    jnz G
    ClearScreen
    movecursor 0104h
    DisplayString Program_Terminated
    mov bx,60h                        ;Testing the drawing
    call SetDelayTime
    jmp far ptr Game_Over
G:
    jmp CheckChar

MoveRight:
    
    mov di,B_Player_pos
    call Get_coordinates
    Delete_Previous_Border
    
    mov al,Black_Pieces_Border_Color
    add cx,22     ;22
    cmp cx,229    ;229
    jna No_Rotate_R
    RotateRight
    
No_Rotate_R:    
    call Get_ScreenPos
    mov B_Player_pos,di    ; Updating White Position

    mov al,Black_Pieces_Border_Color             ; Drawing New Border
    call DrawSquareBorder

    jmp CheckChar

MoveLeft:
    
    mov di,B_Player_pos
    call Get_coordinates
    Delete_Previous_Border

    mov al,Black_Pieces_Border_Color
    sub cx,22
    cmp cx,75
    jnb No_Rotate_L
    RotateLeft

No_Rotate_L:
    call Get_ScreenPos
    mov B_Player_pos,di

    mov al,Black_Pieces_Border_Color
    Call DrawSquareBorder

    jmp CheckChar

MoveDown:
    mov di,B_Player_pos
    call Get_coordinates

    Delete_Previous_Border

    mov al,Black_Pieces_Border_Color
    add dx,22
    cmp dx,157
    jna No_Rotate_D
    RotateDown

No_Rotate_D:
    call Get_ScreenPos
    mov B_Player_pos,di

    mov al,Black_Pieces_Border_Color
    Call DrawSquareBorder

    jmp CheckChar

MoveUp:
    mov di,B_Player_pos
    call Get_coordinates

    Delete_Previous_Border

    mov al,Black_Pieces_Border_Color
    sub dx,22
    cmp dx,3
    jnb No_Rotate_Up
    RotateUp

No_Rotate_Up:
    call Get_ScreenPos
    mov B_Player_pos,di

    mov al,Black_Pieces_Border_Color
    Call DrawSquareBorder

    jmp CheckChar

Move_Right_2:
    mov di,W_Player_Pos
    call Get_coordinates

    Delete_Previous_Border
    
    mov al,White_Pieces_Border_Color
    add cx,22     ;22
    cmp cx,229    ;229
    jna No_Rotate_R2
    RotateRight

No_Rotate_R2:
    call Get_ScreenPos
    mov W_Player_Pos,di

    mov al,White_Pieces_Border_Color
    call DrawSquareBorder

    jmp CheckChar

Move_Left_2:
    mov di,W_Player_Pos
    call Get_coordinates
    Delete_Previous_Border

    mov al,White_Pieces_Border_Color
    sub cx,22
    cmp cx,75
    jnb No_Rotate_L2
    RotateLeft

No_Rotate_L2:
    call Get_ScreenPos
    mov W_Player_Pos,di

    mov al,White_Pieces_Border_Color
    Call DrawSquareBorder

    jmp CheckChar

Move_Down_2:
    mov di,W_Player_Pos
    call Get_coordinates
    Delete_Previous_Border

    mov al,White_Pieces_Border_Color
    add dx,22
    cmp dx,157
    jna No_Rotate_D2
    RotateDown
    
No_Rotate_D2:
    call Get_ScreenPos
    mov W_Player_Pos,di

    mov al,White_Pieces_Border_Color
    Call DrawSquareBorder

    jmp CheckChar

Move_Up_2:
    mov di,W_Player_Pos
    call Get_coordinates
    Delete_Previous_Border

    mov al,White_Pieces_Border_Color
    sub dx,22
    cmp dx,3
    jnb No_Rotate_Up2
    RotateUp
    
No_Rotate_Up2:
    call Get_ScreenPos
    mov W_Player_Pos,di

    mov al,White_Pieces_Border_Color
    Call DrawSquareBorder

    jmp CheckChar

Check_W_Selected_Piece:
    mov al,White_Pieces_Border_Color
    Call DrawSquareBorder

    call White_Pieces_Check_Existing
    jmp CheckChar

Check_B_Selected_Piece:
    mov al,Black_Pieces_Border_Color
    Call DrawSquareBorder

    call Black_Pieces_Check_Existing
    jmp CheckChar

User_Move ENDP 

MainScreen PROC FAR

    ClearScreen

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
         jnb K2
        jmp Checkover

    K2:
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
            ;call TO_DEFAULT   ; To Initialize values with default position

            ;mov bx,40h                        ;Testing the drawing
            ;call SetDelayTime

            call init_board

            mov ax,White_Pieces
            mov Player1_Pieces_Color,ax
            mov ax,Black_Pieces
            mov Player2_Pieces_Color,ax

            ;mov al,0FH
            ;mov bx,16
            ;mov cx,141
            ;mov dx,69
            ;Call Move
;
            ;mov al,0H
            ;mov bx,8
            ;mov cx,185
            ;mov dx,69
            ;Call Move

            ;PushALL
            call User_Move
            ;PopALL

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
Game_Over:

    Call MainScreen

Main ENDP
End main
