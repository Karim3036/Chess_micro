
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

.Model Compact
.Stack 64
.Data

    Deselection             EQU 2
    Valid                   EQU 1
    Invalid                 EQU 0

     whitepieces_flag        db 0
     fromchat                db 0

    real_time_cd_seconds db ?         
    real_time_cd_minutes db ?   

    cd_array_init_seconds   db 16 dup (0)
    
    init_cd_seconds db 0
    
    check_cooldown dw Valid

    cd_seconds equ 3
    
    default_check   dw ?

    Dollar_Sign        equ '$'
    Questionmark_ascii equ '?'

    enter_name             db 'Please Enter your name:','$'
    Player1_name           db  15,?,15 DUP('$') 
    Player2_name           db  15,?,15 DUP('$')    
    cont                   db 'Press any key to continue','$'
    main_menu_chat         db 'To start chatting press F3','$'
    main_menu_game         db 'To start the game press F2','$'
    main_menu_exit         db 'To end the program press ESC','$'
    Error_Message          db 'Error! Name must start with a letter','$'
    Waiting_Message        db 'Invitation sent, wait for response','$'
    Program_Terminated     db 'Program_Terminated','$'
    Check_King             db 'Check','$'
    Check_Mate             db 'Checkmate!!','$'
    Game_time              db 'Game time:','$'
    PlayMessege            db 'Your other party is: ','$'
    ChatInvitationRecieved db '- Chat invitation recieved (F3) ','$'  
    GameInvitationRecieved db '- Game invitation recieved (F2) ','$'

    ChatInvitationSent db '- You sent a chat invitation','$'  
    GameInvitationSent db '- You sent a Game invitation','$'

    P1_Choice db ?
    P2_Choice db ?

    Player_1_Wins   db 'Winner, You WIN!!','$'
    Player_2_Wins   db 'You lost  Hard luck','$'

    side_limit                  dw 4 dup(?)
    Possible_move_boarder       dw 4 dup(?)
    Possible_moves_Size         dw 0
    Possible_moves              dw 21 dup(?),'$'    ;Max possible moves (for Queen) = Queen

    Capture_Opponent_array      dw 21 dup(?)        ;********************************************************************************************

    Capture_White_X_Pos   dw 6
    Capture_White_Y_Pos   dw 3

    Capture_Black_X_Pos   dw 254
    Capture_Black_Y_Pos   dw 157

    Opponent_Troop_Index  dw ?
    selected_Troop_Index  dw ?

    Previous_Selection    dw ?
    Destination_DI        dw ?  

    Boundaries_flag       dw Valid   
    Ally_flag             dw Valid
    opponent_flag         dw Invalid
    Possible_Moves_flag   dw Invalid
    Sent_flag             dw Invalid
    Recieve_flag          dw Invalid
    Timer_flag            dw Invalid
    King_Check_flag       db Invalid
    King_flag_Recieved    db Invalid

    ;; Constants for drawing
    Screen_Width		    equ 320
    Screen_Heigth	 	    equ 200
    
    Sprite_Width	 	       equ 8
    Sprite_Height	 	       equ 5
    squarewidth                equ 21
    Possible_move_boarder_size equ 19
    Num_of_Supported_Chars	   equ 36

    ;******************** Game timer *****************;
    Game_start_time_seconds db 0      
    Game_start_time_minutes db 0
    Game_start_time_hours   db 0   

    Game_end_seconds db 0         
    Game_end_minutes db 0
    Game_end_hours   db 0 

    seconds_passed db 0
    minutes_passed db 0
    hours_passed  db 0

     ;**************** Chat *****************;
    ExitChatMsg		    db 'To end chatting press F3$'
    ChatEndMsg  	    db 'Opponent has exited the chat room$'
    P1Cursor    	    dw 0000h
    P2Cursor    	    dw 0C00h
    P1Cursorin    	    dw 1702h
    P2Cursorin    	    dw 1802h
    line   	db '--------------------------------------------------------------------------------$'
    P1Chat db 70,?,70 dup('$')
    P2Chat db 70,?,70 dup('$')
    ChatVar db 70,?,70 dup('$')
    Val db ?

    Game_Ended EQU 0FFH
    
    ;**************** Game Communication *****************;
    Opponent_Index              dw ?
    Opponent_Destination_X      dw ?
    Opponent_Destination_Y      dw ?

    ;********************* arrays are arranged from the top of the board to the bottom **********************************;
    ;*********************    Black pieces, black pawns, white pawns, white pieces     **********************************;

    Player_Pieces_Color    dw ?
    Opponent_Pieces_Color  dw ?
    temp_Pieces_Color      dw ?
    White_Pieces           EQU 0FH
    Black_Pieces           EQU 0

    black_x_pos  dw 75,229,97,207,119,185,141,163,75,97,119,141,163,185,207,229 
    black_y_pos  dw 8 dup(3),8 dup(25)
    black_DI_val dw 1035,1189,1057,1167,1079,1145,1101,1123,8075,8097,8119,8141,8163,8185,8207,8229

    Black_King_Position    EQU 14
    White_King_Position    EQU 30

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

videomode macro 
 mov ah,0
    mov al,13h
    int 10h    

EndM videomode
 
WaitForSerialInput Macro
	Local CHK

	mov dx , 3FDH ; Line Status Register
        CHK:
            in al , dx
            AND al , 1
            JZ CHK
EndM

CheckForSerialInput Macro	;; Zero flag = 1 if not ready

	mov dx , 3FDH ; Line Status Register
	in al , dx
	AND al , 1

EndM
    

WaitForSerialOutput Macro
	Local AGAIN

            mov dx , 3FDH ; Line Status Register
            AGAIN:
                In al , dx ;Read Line Status
                AND al , 00100000b
                JZ AGAIN

EndM

CheckForSerialOutput Macro	;; Zero flag = 1 if not ready

	mov dx , 3FDH ; Line Status Register
	In al , dx ;Read Line Status
	AND al , 00100000b

EndM

Timer_CD MACRO 
    mov  ah, 2ch   ;get system time
    int  21h       ;ret seconds in DH , minutes in CL , hours in CH
ENDM Timer_CD

DisplayString MACRO STR
    PushALL
    mov Ah,09h
    mov dx, offset STR
    int 21h    
    PopALL
ENDM DisplayString

DisplayStringRead MACRO STR
    mov Ah,09h
    mov dx, offset STR+2
    int 21h    
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

Move_Cursor MACRO X,Y
    mov Ah,02H
    Mov Dl,X
    Mov Dh,Y
    int 10h    
ENDM Move_Cursor

ReadString MACRO PromptMessage             ; READ A STRING                   
    ;DisplayString PromptMessage
    MOV AH,0AH
    MOV DX,OFFSET PromptMessage
    INT 21H
 ENDM ReadString

STRINGCOPY Macro M1,M2,Size
    PushALL
    PUSH DI
    mov AX,DS 
    mov ES,AX
    mov SI,offset M1 
    mov DI,offset M2
    Mov Cx,0
    mov cl,Size
    REP MOVSB
    POP DI
    PopALL
ENDM STRINGCOPY

Clear MACRO 
    mov Ax,3
    int 10h
ENDM Clear  
ClearScreen MACRO 
    PushALL
    mov ax,0600h
    mov bh,0
    mov cx,0
    mov dx,184fh
    int 10h
    PopALL
ENDM ClearScreen

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
ClearStat proc
PushALL             
    mov dx,177          ;start of status bar
status_bar_cols2:
    mov cx,0   
    mov al,0            ;black color
    mov ah,0ch
status_bar_rows2: int 10h
    inc cx
    cmp cx,320
    jnz status_bar_rows
    inc dx
    cmp dx,201
    jb status_bar_cols
PopALL
ret
ClearStat endp

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
    mov Possible_moves_Size,bx                   ;    Possible_moves_Size         dw 0
    mov bl,0
    mov fromchat,bl

    lea si,Possible_moves                        ;    Possible_moves              dw 21 dup(?),'$
    lea bp,Capture_Opponent_array
    mov cx,21                                    ;    Capture_Opponent_array      dw 21 dup(?)   
loop2:
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
    mov Opponent_Troop_Index,ax                 ;    selected_Troop_Index  dw ?
    mov selected_Troop_Index,ax                 ;               
    mov Previous_Selection,ax                   ;    Previous_Selection    dw ?
    mov Destination_DI,ax                       ;    Destination_DI        dw ?             
    
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

lea si,Player1_name               ;Player_name  db  15,?,15 DUP('$')
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

;***************************************************;
;***************** Timer variables******************;
;***************************************************;
mov Ah,0

mov init_cd_seconds,Ah

mov seconds_passed,Ah
mov minutes_passed,Ah
mov hours_passed,AH

mov Game_start_time_seconds,Ah  
mov Game_start_time_minutes,Ah
mov Game_start_time_hours,Ah   

mov Game_end_seconds,Ah
mov Game_end_minutes,Ah
mov Game_end_hours,Ah

mov cx,8
ARRAY_INIT:
mov ax,0
LEA SI, cd_array_init_seconds
mov [SI],ax
loop ARRAY_INIT

;move zeros in registers
xor ax,ax
xor bx,bx
xor cx,cx
xor dx,dx

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
;...............................................................;
;                        DRAWING AREA                           ;
;...............................................................;
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

;...............................................................;
;                     END OF DRAWING AREA                       ;
;...............................................................;

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
    mov bx,0        ;Index for array

Start_the_check: 

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

    PushALL
    mov di,Previous_Selection                ; Deletes Previous selection border
    call Get_coordinates

    mov ah,0DH
    mov bh,0
    int 10h

    call Draw_possiblemove_Border
    PopALL

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

    ;call clear_status_bar
    ;mov King_Check_flag,Invalid
    PopALL
ret
Delete_selection endp

;**********************************************************************;
;******************  Start of Troops Limitations   ********************;
;**********************************************************************;

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
    PushALL
    mov ax,Opponent_Troop_Index
    mov [bp],ax
    add bp,2
    PopALL

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
    PushALL
    mov ax,Opponent_Troop_Index
    mov [bp],ax
    add bp,2
    PopALL

endit:
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
    push di
Check_Forward_Movement:
    sub di,7040

    call Check_In_boundaries 
    cmp Boundaries_flag,Invalid
    jz blah

    call Check_ally
    cmp Ally_flag,Valid 
    jz blah

    call Check_opponent
    cmp opponent_flag,Valid
    jz blah
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx    
    jmp Check_double_forwrd_move   
blah:
    ;sub di,7040
  
    jmp Check_Right_Movement

Check_double_forwrd_move:
    pop di
    push di
    sub di,14080

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
    cmp dx,91
    jb Check_Right_Movement
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

    
Check_Right_Movement:
    pop di
    push di
    sub di,7018

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
    pop di
    push di
    sub di,7062

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
    pop di
    PopALL
ret
white_pawn_limitations endp 

Black_pawn_limitations proc far
    PushALL

    call Get_ScreenPos
    lea si,Possible_moves
    mov ax,0
    mov Possible_moves_Size,ax
    mov bx,0
    push di
Check_Forward_Movement2:
    add di,7040

    call Check_In_boundaries 
    cmp Boundaries_flag,Invalid
    jz blah2 

    call Check_ally
    cmp Ally_flag,Valid 
    jz blah2

    call Check_opponent
    cmp opponent_flag,Valid
    jz blah2
    ;(if not put in possible moves else jmp over it)
    
    call Get_coordinates    ;Draws border in the possible move
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx
    jmp Check_double_forwrd_move2
blah2:
    ;add di,7040   
    jmp Check_Right_Movement2

Check_double_forwrd_move2:
    pop di
    push di
    add di,14080

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
    mov al,9
    call Draw_possiblemove_Border
    
    mov [SI],di
    add si,2
    inc bx

    
Check_Right_Movement2:
    pop di
    push di
    add di,7018
    
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
    pop di
    push di
    add di,7062

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
    pop di
    PopALL
ret
Black_pawn_limitations endp 


;**********************************************************************;
;*******************  End of Troops Limitations   *********************;
;**********************************************************************;
;*********************************************;
;************ chat mode functions ************;
;*********************************************;

InitializationSerial Proc
    mov dx,3fbh 			; Line Control Register
    mov al,10000000b		;Set Divisor Latch Access Bit
    out dx,al				;Out it
    mov dx,3f8h			
    mov al,0ch			
    out dx,al

    mov dx,3f9h
    mov al,00h
    out dx,al

    mov dx,3fbh
    mov al,00011011b
    out dx,al

    ret
InitializationSerial ENDP 

sendChar proc 
;Check that Transmitter Holding Register is Empty

        mov dx , 3FDH		; Line Status Register
AGAIN1:  	In al , dx 			;Read Line Status
  		test al , 00100000b
  		JZ AGAIN1                              ;Not empty

;If empty put the VALUE in Transmit data register
  		mov dx , 3F8H ; Transmit data register
  		mov al,Val
        out dx , al		
ret
sendChar endp

recChar proc
;Check that Data is Ready

        mov dx , 3FDH		; Line Status Register
	CHKS:	in al , dx 
  		test al , 1
  		JZ CHKS                                   ;Not Ready
 ;If Ready red the VALUE in Receive data register
  		mov dx , 03F8H 
  		in al , dx
  		mov VAL,al
        
ret
recChar endp

send proc near
;Check that Transmitter Holding Register is Empty

        mov si,2
  		looping:
        mov dx , 3FDH		; Line Status Register
AGAIN:  	In al , dx 			;Read Line Status
  		test al , 00100000b
  		JZ AGAIN                             ;Not empty

;If empty put the VALUE in Transmit data register

  		mov dx , 3F8H ; Transmit data register
  		mov al,ChatVar[si]
  		inc si
        out dx , al 
        loop looping
       
ret
send endp

recieve proc near
;Check that Data is Ready

        mov si,2 
  		looping1:
        mov dx , 3FDH		; Line Status Register
	CHK:	in al , dx 
  		test al , 1
  		JZ CHK                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H

  		in al , dx
  		mov ChatVar[si],al
        inc si
        loop looping1
        ret
recieve endp

Start_Chatting proc far

    Clear
    Move_Cursor 0,23
    DisplayString line
    Move_Cursor 0,24
    DisplayString ExitChatMsg
    Call InitializationSerial
    
    ;Drawing a horz line at the middle if the Screen 
    Move_Cursor 0,11
    DisplayString line
        
    movecursor P1Cursor
    DisplayStringRead Player1_name
    movecursor P2Cursor
    DisplayStringRead Player2_name

     
    mov bl,1 ; y-coordinates

   ;player 1
ChatP1:
    Move_Cursor 0,10
    
     
    mov ah,0    ;Check F3 Not pressed
    int 16h
    mov Val,ah
    call sendChar
    cmp ah,3Dh  ;F3
    jnz Noo_Exit_Chat
    
    jmp far ptr chat2main
Noo_Exit_Chat: 
    Move_Cursor 0,10  ;read from location
    ReadString P1Chat
;    Push Cx
;    Mov Cl,4
;    SHR P1Chat,Cl
;    Pop CX
;    Mov P1Chat,AL ;Move First Charachter  
    Move_Cursor 2,bl
    DisplayStringRead P1Chat
    STRINGCOPY P1Chat,ChatVar,70
    mov cx,70
    
    call send

    PushALL
     
    mov si,Offset P1Chat+2  ;String to default
    mov cx,70
loooop:
    mov al,Dollar_Sign  
    mov [si],al
    inc si
    dec cx
    jnz loooop
        
        mov ax,0600h ;Clear the TextBox
        mov bh,07 
        mov cx,0A00H ;(0,10)
        mov dx,0A50H ;(80,10)
        int 10h    
    PopALL
    
    ;Player2 
ChatP2: 
    
    
    call recChar    
    mov ah,Val
    cmp ah,3Dh
    jnz Noo_Exit_Chat1  
    inc fromchat
    jmp far ptr chat2main
Noo_Exit_Chat1: 
    mov cx,70
    call recieve
    STRINGCOPY ChatVar,P2Chat,70
    add bl,12
    Move_Cursor 2,bl
    DisplayStringRead P2Chat
    sub bl,12
    PushALL
     
    mov si,Offset P2Chat+2  ;String to default
    mov cx,70
looop:
    mov al,Dollar_Sign
    mov [si],al
    inc si
    dec cx
    jnz looop
   
        mov ax,0600h     ;Clear the TextBox
        mov bh,07 
        mov cx,1500h ;(0,21)
        mov dx,1550h ;(80,21)
        int 10h
    PopALL
    inc bl

    
    cmp bl,8H           ;Scrolling
    jnc ScrollChat
    jmp ChatP1


ScrollChat:
    PushALL
        mov ax,0603h 
        mov bh,07 
        mov cx,0100H ;(0,1)
        mov dx,0A50H ;(80,10)
        int 10H
        
        mov ax,0603h 
        mov bh,07 
        mov cx,0D00h ;(0,13)
        mov dx,1550h ;(80,21)
        int 10h

    PopALL

        sub bl,3
        jmp ChatP1
Start_Chatting endp

Inline_Chatting proc far
PushALL
    PushALL
    mov al,3FH
    mov Val,al
    call sendChar
    PopALL
    ;Clear
    ;Move_Cursor 0,23
    ;DisplayString line
    ;Move_Cursor 0,24
    ;DisplayString ExitChatMsg
    Call InitializationSerial
    call clear_status_bar
    ;Drawing a horz line at the middle if the Screen 
    ;Move_Cursor 0,11
    ;DisplayString line
        
    movecursor P1Cursorin
    DisplayStringRead Player1_name
    movecursor P2Cursorin
    DisplayStringRead Player2_name

     
    ;mov bl,1 ; y-coordinates

   ;player 1
ChatP1in:
    ;Move_Cursor 0,10
    
     
    mov ah,0    ;Check F3 Not pressed
    int 16h
    mov Val,ah
    call sendChar
    cmp ah,3fh  ;F3
    jnz Noo_Exit_Chatin
    
    jmp frominline
Noo_Exit_Chatin: 
    movecursor 1703h ;read from location
    ReadString P1Chat
;    Push Cx
;    Mov Cl,4
;    SHR P1Chat,Cl
;    Pop CX
;    Mov P1Chat,AL ;Move First Charachter  
    movecursor 1703h
    DisplayStringRead P1Chat
    STRINGCOPY P1Chat,ChatVar,70
    mov cx,70
    
    call send

    PushALL
     
    mov si,Offset P1Chat+2  ;String to default
    mov cx,70
loooopin:
    mov al,Dollar_Sign  
    mov [si],al
    inc si
    dec cx
    jnz loooopin
        
        ;mov ax,0600h ;Clear the TextBox
        ;mov bh,07 
        ;mov cx,0A00H ;(0,10)
        ;mov dx,0A50H ;(80,10)
        ;int 10h    
    PopALL
    
    ;Player2 
ChatP2in: 
    
    movecursor 1803h
    call recChar    
    mov ah,Val
    cmp ah,3fh
    jnz Noo_Exit_Chat1in  
    ;inc fromchat
    jmp frominline
Noo_Exit_Chat1in: 
movecursor 1803h
    mov cx,70
    call recieve
    STRINGCOPY ChatVar,P2Chat,70
    ;add bl,12
    ;Move_Cursor 2,bl
    DisplayStringRead P2Chat
    ;sub bl,12
    PushALL
     
    mov si,Offset P2Chat+2  ;String to default
    mov cx,70
looopin:
    mov al,Dollar_Sign
    mov [si],al
    inc si
    dec cx
    jnz looopin
   
        ;mov ax,0600h     ;Clear the TextBox
        ;mov bh,07 
        ;mov cx,1500h ;(0,21)
        ;mov dx,1550h ;(80,21)
        ;int 10h
    PopALL
    ;inc bl

    
    ;cmp bl,8H           ;Scrolling
    ;jnc ScrollChat
    jmp ChatP1in


;ScrollChat:
;    PushALL
;        mov ax,0603h 
;        mov bh,07 
;        mov cx,0100H ;(0,1)
;        mov dx,0A50H ;(80,10)
;        int 10H
        
 ;       mov ax,0603h 
 ;       mov bh,07 
 ;       mov cx,0D00h ;(0,13)
 ;       mov dx,1550h ;(80,21)
 ;       int 10h

    ;PopALL

   ;     sub bl,3
        jmp ChatP1in
    frominline:
    PopALL
        ret

Inline_Chatting endp

;****************************************************;
;****************End of chat functions **************;
;****************************************************;


print_number proc far
PushALL             ;put value in ax before calling

;initialize count
    mov cx,0
    mov dx,0
    label1:
        ; if ax is zero
        cmp ax,0
        je print1     
         
        ;initialize bx to 10
        mov bx,10       
         
        ; extract the last digit
        div bx                 
         
        ;push it in the stack
        push dx             
         
        ;increment the count
        inc cx             
         
        ;set dx to 0
        xor dx,dx
        jmp label1
    print1:
        ;check if count
        ;is greater than zero
        cmp cx,0
        je exit
         
        ;pop the top of stack
        pop dx
         
        ;add 48 so that it
        ;represents the ASCII
        ;value of digits
        add dx,48
         
        ;interrupt to print a
        ;character
        mov ah,02h
        int 21h
         
        ;decrease the count
        dec cx
        jmp print1
exit:

PopALL
ret
print_number ENDP

Get_total_game_time proc far
PushALL
        Timer_CD
         mov Game_end_seconds,dh         
         mov Game_end_minutes,cl
         mov Game_end_hours,ch

         mov ah,Game_end_seconds
         cmp ah,Game_start_time_seconds
         jb next_minute
         mov ah,Game_end_seconds
         sub ah,Game_start_time_seconds
         mov seconds_passed,ah
         jmp calc_minutes

         next_minute:
         dec Game_end_minutes
         mov ah,Game_end_seconds
         add ah,60
         sub ah,Game_start_time_seconds
         mov seconds_passed,ah
         
         calc_minutes:
         mov ah,Game_end_minutes
         cmp ah,Game_start_time_minutes
         jb next_hour
         sub ah,Game_start_time_minutes
         mov minutes_passed,ah
         jmp calc_hours

         next_hour:
         dec Game_end_hours
         mov ah,Game_end_minutes
         add ah,60
         sub ah,Game_start_time_minutes
         mov minutes_passed,ah
            
         calc_hours:
         mov ah,Game_end_hours
         sub ah,Game_start_time_hours
         mov hours_passed,ah

         PopALL
ret
Get_total_game_time ENDP

Disp_game_time proc far
PushALL
 movecursor 1704h
         mov ah,2
         mov dl,'0'
         int 21h
         

         mov ax,0
         mov al,hours_passed
         call print_number
         mov ah,2
         mov dl,':'
         int 21h

         mov ah,2
         mov dl,'0'
         int 21h

         mov ax,0
         mov al, minutes_passed
         call print_number
         mov ah,2
         mov dl,':'
         int 21h

         mov ax,0
         mov al, seconds_passed
         call print_number
         
PopALL
ret
Disp_game_time endp

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
    mov Previous_Selection,di
    mov al,1
    call Draw_possiblemove_Border
    PopALL

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

Piece_is_White_Pawn:
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

Return:  
    cmp Possible_moves_Size,0
    jnz No_Border_Deletion

    PushALL
    call Get_ScreenPos                ;Gives di of selected troop
    mov Previous_Selection,di
    mov ah,0Dh
    int 10h
    call Draw_possiblemove_Border
    PopALL

No_Border_Deletion:   
ret
White_Pieces_Check_Existing endp

Black_Pieces_Check_Existing proc far

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
    ja S3
    jmp My_Piece_Checks_Black
S3:
    jmp Return2

My_Piece_Exists_Black:                      ;Index of piece is in ax
    PushALL
    call Get_ScreenPos                ;Gives di of selected troop
    mov Previous_Selection,di
    mov al,1
    call Draw_possiblemove_Border
    PopALL

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

Piece_is_Black_Pawn:
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
    ; Only 1 Queen
    call Queen_limitations
    jmp Return2

Piece_is_Black_King:
    ; Only 1 king
    call king_limitations
    jmp Return2

Return2:
    cmp Possible_moves_Size,0
    jnz No_Border_Deletion2

    PushALL
    call Get_ScreenPos                ;Gives di of selected troop
    mov Previous_Selection,di
    mov ah,0Dh
    int 10h
    call Draw_possiblemove_Border
    PopALL

No_Border_Deletion2:
ret
Black_Pieces_Check_Existing endp 


Check_Captured_Ally proc
    PushALL

    mov bx,0 ; For troop index
    mov cx,Opponent_Destination_X
    mov dx,Opponent_Destination_Y
    call Get_ScreenPos   ; Returns di of the opponents new move

    cmp Player_Pieces_Color,White_Pieces
    jz Ally_White

    lea si,black_DI_val
    mov al,0
    mov cx,Capture_Black_X_Pos
    mov dx,Capture_Black_Y_Pos
    jmp Continue3

Ally_White:
    lea si,white_DI_val
    mov al,0FH
    mov cx,Capture_White_X_Pos
    mov dx,Capture_White_Y_Pos

Continue3:
    cmp di,[si]
    jz Captured

    add si,2
    add bx,2
    cmp bx,30
    ja No_Capture
    jmp Continue3

Captured:
    ; fn move can be called as al=color , bx=index, cx=new X, dx=new Y
    call Move
    cmp Player_Pieces_Color,White_Pieces
    jz Add_White

    sub Capture_Black_Y_Pos,18
    cmp Capture_Black_Y_Pos,20
    jnb No_Capture
    mov Capture_Black_X_Pos,272
    mov Capture_Black_Y_Pos,150
    jmp No_Capture

Add_White:

    add Capture_White_Y_Pos,18
    cmp Capture_White_Y_Pos,129
    jna No_Capture
    mov Capture_White_X_Pos,24
    mov Capture_White_Y_Pos,3

No_Capture:

    PopALL
ret
Check_Captured_Ally endp

;********************************************************************;
;                  Check if the movement made is valid               ;
;********************************************************************;
Valid_Movement proc far  
    ; cx & dx contains the destination position
    PushALL

    ;********************************** Troop will move *************************************;

    PushALL
    call Get_total_game_time
    call Disp_game_time
    mov di,Previous_Selection                ; Deletes Previous selection border
    call Get_coordinates

    mov ah,0DH
    mov bh,0
    int 10h

    call Draw_possiblemove_Border
    PopALL

    mov ax,Black_Pieces
    lea si,Player_Pieces_Color
    cmp ax,[si]
    jz white_opp

    lea si,black_DI_val
    jmp Dummy

white_opp:
    lea si,white_DI_val

Dummy:
    call Get_ScreenPos   ; Returns di of destination
    mov Destination_DI,di
    call Check_opponent
    cmp opponent_flag,Valid
    jz opponent_there

    jmp Start2

opponent_there:
    mov ax,Possible_moves_Size
    push ax
    mov ax,0

    array_loop:

        cmp di,[si]
        jz capture
        ;loop possible moves array
        ;law el index gwah yeb2a opponent
        ;get_coordinates
        ;draw ally henak
        add si,2
        add ax,2
        dec Possible_moves_Size
        jnz array_loop
        
    capture:
        mov bx,Opponent_Troop_Index    ;Giving bx the index of the captured array

        pop ax
        mov Possible_moves_Size,ax

        PushALL

        mov ax,Black_Pieces       ;get player color
        lea si,Player_Pieces_Color
        cmp [si],ax
        jnz nxtjmp
        jmp Black_Color
        nxtjmp:
        cmp Opponent_Troop_Index,14
        jz Jaja
        jmp Dummm
    Jaja:    
        PushALL
        mov al,Game_Ended
        mov Val,al
        call sendChar
        PopALL
        
        call Get_total_game_time
        call clear_status_bar
        movecursor 1712h
        DisplayString Check_Mate

        mov bx,40h
        call SetDelayTime

        ClearScreen
        movecursor 0404h
        DisplayString Player_1_Wins       ; White wins

        movecursor 1502h
        DisplayString Game_time
        ;call Get_total_game_time
        call Disp_game_time

        mov bx,60h                        ;Testing the drawing
        call SetDelayTime

        jmp far ptr Game_Over     ;Returns to MainScreen

    Dummm:
        mov al,00H
        mov cx,Capture_Black_X_Pos
        mov dx,Capture_Black_Y_Pos
        
        sub Capture_Black_Y_Pos,18
        cmp Capture_Black_Y_Pos,20
        jnb Du
        mov Capture_Black_X_Pos,272
        mov Capture_Black_Y_Pos,150

    Du:
        jmp White_Color9

    Black_Color:

        cmp Opponent_Troop_Index,30
        jnz Dummm2

        ClearScreen
        movecursor 0404h
        DisplayString Player_1_Wins

        movecursor 1502h
        DisplayString Game_time
        call Get_total_game_time
        call Disp_game_time

        mov bx,60h                        ;Testing the drawing
        call SetDelayTime

        jmp far ptr Game_Over     ;Returns to MainScreen

    Dummm2:
        mov al,0FH
        mov cx,Capture_White_X_Pos
        mov dx,Capture_White_Y_Pos

        add Capture_White_Y_Pos,18
        cmp Capture_White_Y_Pos,129
        jna White_Color9
        mov Capture_White_X_Pos,24
        mov Capture_White_Y_Pos,3

    White_Color9:
        Call Move   ;Draw the captured troop outside the grid
        PopAll

Start2:
    ;call Get_coordinates      ;selected piece draw
    mov di,Destination_DI
    call Get_coordinates

    mov ax,Black_Pieces       ;get player color
    lea si,Player_Pieces_Color
    cmp [si],ax
    jz Black_Player

    mov al,0FH
    jmp White_Player

Black_Player:
    mov al,0H

White_Player:
    mov bx,selected_Troop_Index
    PushALL

    lea si,cd_array_init_seconds
    mov ax,selected_Troop_Index
    shr ax,1
    add si,ax
    Timer_CD                        ;get system time
    mov [si],dh           ;ret seconds in DH , minutes in CL , hours in CH
    
    PopALL
    call Move
   
Return_To_Select:

    call Delete_selection
    PopALL
    
ret
Valid_Movement endp  

;***********************************************************;
;              Cooldown time for pieces = 3 secs            ;
;***********************************************************;

CoolDown_Check proc 

    PushALL
    lea si,cd_array_init_seconds
    mov ax,selected_Troop_Index
    shr ax,1
    add si,ax
     
    Timer_CD   
    mov real_time_cd_seconds,dh
    
    mov bh,real_time_cd_seconds 
    cmp bh,[si]
    jb clr_init_cd_seconds
    jmp go_to_check
    
clr_init_cd_seconds:
    mov bh,0
    mov [si],bh 
    
go_to_check:
    
    ;mov cd_seconds,3
    mov bh,real_time_cd_seconds
    sub bh,[si]
    cmp bh,cd_seconds
    ja Troop_Can_Move
    mov ax,Invalid
    jmp FunctionEnd

Troop_Can_Move:
    mov ax,Valid

FunctionEnd:
    mov Timer_flag,ax    
    PopALL 

ret
CoolDown_Check endp

;***********************************************************;
;                 Checking Check for the king               ;
;***********************************************************;

Check_For_King proc 

    PushALL
    mov al,Invalid
    cmp Possible_moves_size,0
    je King_Check_Done

    mov ax,Black_Pieces       ;get player color
    cmp ax,Player_Pieces_Color
    jz White_King

    mov bx,Black_King_Position
    lea si,black_DI_val
    mov ax,[si + bx]     ;King's di value
    lea si,Possible_moves
    mov cx,Possible_moves_Size   ;Counter for possible moves array

    jmp Check_King_Loop

White_King:
    mov bx,White_King_Position
    lea si,white_DI_val
    add si,bx
    mov ax,[si]     ;King's di value
    lea si,Possible_moves
    mov cx,Possible_moves_Size   ;Counter for possible moves array

Check_King_Loop:     ;Checks If the king can be killed (appears in possible moves)
    cmp ax,[si]
    jz King_Found

    add si,2
    Loop Check_King_Loop

    jmp No_King

King_Found:
    mov al,valid
    call clear_status_bar

    movecursor 1712h
    DisplayString Check_King
    jmp King_Check_Done

No_King:
    mov al,Invalid
    call clear_status_bar

King_Check_Done:
    mov King_Check_flag,al
    PopAll
ret
Check_For_King endp

;***********************************************************;
;        Function To set possible moves of any piece        ;
;***********************************************************;

Check_Possible_Move proc
PushALL
    call Get_ScreenPos         ;Gives di of destination
    mov Destination_DI,di      ;Saves the destination position

    cmp di,Previous_Selection
    jnz Dummyjmp6
    jmp Deselect

Dummyjmp6:

    lea si,Possible_moves
    mov ax,0

Loop_Possible_moves:     ; Checks if the Q pressed is a possible move

    cmp [si],di
    je Return_Valid

    add si,2
    inc ax      ;Counter for possible moves array
    cmp ax,Possible_moves_Size
    jbe Loop_Possible_moves
    jmp Return_Not_Valid

Deselect:
    mov ax,Deselection
    jmp Check_Over

Return_Valid:
    mov ax,Valid
    jmp Check_Over    

Return_Not_Valid:
    mov ax,Invalid

Check_Over:
    mov Possible_Moves_flag,ax
PopALL
ret
Check_Possible_Move endp

;************************************************************************;
;                        Game / Chat Main Loop                           ;
;************************************************************************;
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
    call Get_total_game_time
    call Disp_game_time

    mov al,0AH
    call DrawSquareBorder   ;To Draw the border with every move

    mov ah,1                            ;Waiting for any key to be pressed
    int 16h
    jnz Hummy
    jmp Check_Recieved

Hummy:
    mov ah,0
    int 16h

    cmp ah,32        ;d
    jnz H
    jmp MoveRight

H:    
    cmp ah,30        ;a
    jnz M
    jmp MoveLeft

M:    
    cmp ah,31        ;s
    jnz S
    jmp MoveDown
S:
    
    cmp ah,17        ;w
    jnz W
    jmp MoveUp
W:    
    cmp ah,16        ;Q
    jnz F1
    cmp Possible_moves_size,0
    jz No_Moves
    ; Here we should check if the pressed Q is in a possible move
    call Check_Possible_Move
    cmp Possible_Moves_flag,Deselection  ; Means that the player have pressed on the selected piece again
    jz Delete_Previous_Selection

    cmp Possible_Moves_flag,Invalid   ; Means that the player have pressed on a wrong position to move to
    jz CheckChar

    call CoolDown_Check
    cmp Timer_flag,Valid   ; Means that the player is trying to move the same piece twice in 3 seconds -> Invalid Movement
    jz Send_Move
    call Delete_selection
    jmp CheckChar
F1:
    jmp F    
; Sending The move to the other player
Send_Move:
    PushALL
    mov ax,selected_Troop_Index
    mov Val,al
    call sendChar   ; Index Sent
    PopALL

    PushALL
    mov ax,cx
    mov Val,al
    call sendChar   ; X position sent
    PopALL

    PushALL
    mov ax,dx
    mov Val,al
    call sendChar   ; Y position sent
    PopALL

    call Valid_Movement      ; Valid Movement by the player
    mov di,Destination_DI
    call Get_coordinates        ;To start the next loop from the destination position
    jmp CheckChar

Delete_Previous_Selection:
    call Delete_selection   ; Previous Selection variable is filled
    jmp CheckChar

No_Moves:
    jmp Check_Selected_Piece
F:
    cmp ah,3EH       ;F4   Terminates Program
    jnz G

    mov Val,ah
    call sendChar

    ClearScreen
    movecursor 0404h
    DisplayString Program_Terminated

    call Get_total_game_time
    call Disp_game_time

    mov bx,60h                        ;Testing the drawing
    call SetDelayTime
    jmp far ptr Game_Over
G:
    cmp ah,3FH   ; F5 for inline
    jz Inline
    jmp CheckChar
Inline:
    call Inline_Chatting
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
;********************   Checking if the opponent has made a move   *************************;
Check_Recieved:
    PushALL
    mov dx , 3FDH		; Line Status Register
    in al , dx 
  	test al , 1
  	jnz Ready                                   ;Not Ready

    PopALL
    jmp CheckChar
;If Ready read the VALUE in Receive data register
Ready:
    mov dx , 03F8H
	in al , dx
    cmp al,Game_Ended
    jnz No_Winner_Yet

    ClearScreen
    movecursor 0404h
    DisplayString Player_2_Wins

    movecursor 1502h
    DisplayString Game_time
    call Get_total_game_time
    call Disp_game_time

    mov bx,60h                        ;Testing the drawing
    call SetDelayTime

    jmp far ptr Game_Over     ;Returns to MainScreen

No_Winner_Yet:    
    cmp al,3EH  ; F4
    jnz No_Terminate

    ClearScreen
    movecursor 0404h
    DisplayString Program_Terminated

    call Get_total_game_time
    call Disp_game_time

    mov bx,60h
    call SetDelayTime
    jmp far ptr Game_Over
    
No_Terminate:
    cmp al,3FH ; F5
    jnz No_Inline
    call Inline_Chatting
    jmp CheckChar
    
No_Inline:
    mov ah,0
    mov Opponent_Index,ax     ; Index recieved and saved

    PushALL
    call recChar
    mov al,Val
    mov ah,0
    mov Opponent_Destination_X,ax    ; X position of destination recieved and saved 
    PopALL

    PushALL
    call recChar
    mov al,Val
    mov ah,0
    mov Opponent_Destination_Y,ax   ; Y position of destination recieved and saved 
    PopALL

    call Check_Captured_Ally

    mov ah,0
    mov ax,Opponent_Pieces_Color
    mov bx,Opponent_Index
    mov cx,Opponent_Destination_X
    mov dx,Opponent_Destination_Y
    call Move ; Makes the opponent's move on my grid

;******************** King Check ************************;
    PushALL
    mov ax,Player_Pieces_Color     ; In order to enter the the functions as my opponent
    mov temp_Pieces_Color,ax       ;
    mov ax,Opponent_Pieces_Color   ;
    mov Player_Pieces_Color,ax     ;

    cmp Opponent_Pieces_Color,Black_Pieces
    jz Black_Opponent

    call White_Pieces_Check_Existing  ; Returns with possible moves
    jmp dada

Black_Opponent:
    call Black_Pieces_Check_Existing

dada:
    call Check_For_King

No_King2:
    call Delete_selection
    mov ax,temp_Pieces_Color    ; To restore my original color
    mov Player_Pieces_Color,ax  ;
    PopALL
;******************** End of King Check ************************;
    PopALL
    jmp CheckChar

; ***********************  End of Recieveing  ************************ ;

Check_Selected_Piece:
    mov al,0AH
    Call DrawSquareBorder

    mov ax,Black_Pieces
    lea si,Player_Pieces_Color
    cmp [si],ax
    jz Black_Pieces_Checked2

    call White_Pieces_Check_Existing
    jmp CheckChar

Black_Pieces_Checked2:
    call Black_Pieces_Check_Existing
    jmp CheckChar

User_Move ENDP 

MainScreen PROC FAR

    videomode
    ClearScreen

    NameEntry:
         movecursor 0604h
         DisplayString enter_name
         movecursor 0704h
         ReadString Player1_name
         mov si,offset Player1_name
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
     ; Exchange Names
         
         
          ;Recieve Name
          Mov Cx,15
          Call recieve
          STRINGCOPY ChatVar,Player2_name,15

         ;Send Name 
         STRINGCOPY Player1_name,ChatVar,15
         Mov Cx,15
         Call send
         movecursor 1405h
         DisplayString cont                        ; displays "press any key to continue"

         mov ah,0
         int 16h    
    chat2main: 
         videomode     
         ClearScreen
         cmp fromchat,Invalid
         jz normalstart
         Move_Cursor 2,23
         DisplayString ChatEndMsg 

    normalstart:
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
         Mov P1_Choice,0
         Mov P2_Choice,0

     Check_Send_P1:
            mov ah,0
            int 16h
            mov P1_Choice,ah  ; Saving the pressed button of player 1

            mov Val,ah
            call sendChar
    
            CMP P1_Choice,1 ; ESC Scan code
            JNZ NoExit
            JMP ProgramEnd
            NoExit:

            CMP P1_Choice,3DH  ; F3 scan code
            JZ P1StartChat

            CMP P1_Choice,3CH  ; F2 scan code
            JZ P1StartGame

        P1StartChat:
            mov al,P1_Choice
            CMP al,P2_Choice  ; Check invitation from player 2
            jnz Humm3
            jmp chat

        Humm3:
           
            ;Messege in player1 Notification that invitation is sent
            call ClearStat
            Move_Cursor 0,23
            DisplayString ChatInvitationSent  

            JMP Check_Send_P2

        P1StartGame:
            mov al,P1_Choice
            cmp al,P2_Choice
            jnz Humm4 
            jmp game

            
        Humm4:
            call ClearStat
            Move_Cursor 0,23
            DisplayString GameInvitationSent  
            mov whitepieces_flag,1
            ;JMP Check_Send_P2 

        Check_Send_P2:
            call recChar
            mov al,Val
            mov P2_Choice,al

            cmp P2_Choice,1  ; ESC
            jnz Dummyyy
            jmp ProgramEnd

        Dummyyy:
            cmp P2_Choice,3DH
            jz P2StartChat

            cmp P2_Choice,3CH
            jz P2StartGame  

        P2StartGame:
            mov al,P1_Choice
            cmp al,P2_Choice
            jnz ta
            jmp game 
ta:
            
            ;Messege in player1 Notification that invitation is sent
            call ClearStat
            Move_Cursor 0,23
            DisplayString GameInvitationRecieved  
            jmp Check_Send_P1   

        P2StartChat:
            mov al,P1_Choice
            cmp al,P2_Choice
            jz chat

            
            ;Messege in player1 Notification that invitation is sent
            call ClearStat
            Move_Cursor 0,23
            DisplayString ChatInvitationRecieved  
            jmp Check_Send_P1  

        chat:  
             
             
             call Start_Chatting
             jmp MainEnd
        game:
            videomode
            call TO_DEFAULT   ; To Initialize values with default position

            call init_board


              ;---------start game timer-----------;
             PushALL
            
            Timer_CD
             mov Game_start_time_seconds,dh
             mov Game_start_time_minutes,cl
             mov Game_start_time_hours,ch
              PopALL 
             ;---------------------------------;

             cmp whitepieces_flag,1
             jnz black_player_color

             mov ax,White_Pieces
             mov Player_Pieces_Color,ax
             mov ax,Black_Pieces
             mov Opponent_Pieces_Color,ax
             jmp chess_game

        black_player_color:
             mov ax,Black_Pieces
             mov Player_Pieces_Color,ax
             mov ax,White_Pieces
             mov Opponent_Pieces_Color,ax

        chess_game:
            call User_Move

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
    
    videomode                                ; change to video mode
    Call InitializationSerial
Game_Over:

    Call MainScreen

Main ENDP
End main
