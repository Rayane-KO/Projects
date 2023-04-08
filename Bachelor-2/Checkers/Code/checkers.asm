; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Elliott Octave, Rayane Kouidane
; date:	1/10/2020
; program:	Checkers
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

;Constants
TRUE EQU 1
FALSE EQU 0
W EQU 'w'   ;white checker
WK EQU 'W'  ;white king
B EQU 'b'   ;black checker
BK EQU 'B'  ;black king
X EQU 'x'   ;empty
Z EQU 'z'   ;empty (a piece can never be on this square)
LOWER_LIMIT EQU 0
UPPER_LIMIT EQU 256
FIRST_ROW EQU 28
LAST_ROW EQU 224
ROWS EQU 8
COLUMNS EQU 8
JUMP1 EQU 28 ;single jump left down / right up
JUMP2 EQU 36 ;single jump right down / left up
NO_POSITION EQU -1
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 320	; screen witdth
SCRHEIGHT EQU 200	; screen height
SQRWIDTH EQU 40     ; square width
SQRHEIGHT EQU 25    ; square height
FULLPALLETESIZE EQU 768 ; bytes in palette
COLORCOUNT EQU 128 ; number of unique colors for palette cycling
PIXELCOUNT1 EQU SCRWIDTH*SCRHEIGHT	; pixel count
PIXELCOUNT2 EQU SQRWIDTH*SQRHEIGHT	; pixel count

CODESEG
;------------------------------------------------------------------------------------
;Logic procedures
;------------------------------------------------------------------------------------

;;CheckMove takes a pointer to the board, the current position and the next position.
;;It checks if the move from current to next is valid or not.
;;If it's valid ax will be TRUE otherwise it will be FALSE
PROC CheckMove 
    ARG @@arrayptr:dword, @@current:dword, @@next:dword
	USES ebx, ecx, esi, edi
	;check if you want the move is beyond the limit
	cmp [@@current], LOWER_LIMIT
	jl @@wrongmove
	cmp [@@current], UPPER_LIMIT
	jg @@wrongmove
	cmp [@@next], LOWER_LIMIT
	jl @@wrongmove
	cmp [@@next], UPPER_LIMIT
	jg @@wrongmove
	
	;eax: pointer to the current position in the board
	;ebx: pointer to the next position in the board
    mov eax, [@@current]
    add eax, [@@arrayptr]
    mov ebx, [@@next]
    add ebx, [@@arrayptr]
	
	;check if there is a piece that must eat
	cmp [musteatPos1], -1
	jne @@MustMove
	
	;depending on the turn, white or black will play
	@@checkColor:
	cmp [currentTurn], 0
	jl @@checkWhite
	jg @@checkBlack
	
	;this will make sure that we can only move the piece that must eat
	@@MustMove:
	mov [onlyEat], 1    ;with this we can say that we can move the piece 
	mov edi, [@@current];only by eating (you can't make a simple jump)
	cmp [musteatPos1], edi
	je @@checkColor
	cmp [musteatPos2], edi
	je @@checkColor
	jmp @@wrongmove
	
	@@checkWhite:
   	mov edx, W
	cmp [eax], edx 
	je @@whitecheck
	Mov edx, WK
	cmp [eax], edx
	je @@WKing
	jmp @@wrongmove
	
	@@checkBlack:
	mov edx, B
	cmp [eax], edx
	je @@blackcheck
	Mov edx, BK
	cmp [eax], edx
	je @@BKing
	Jmp @@wrongmove

	@@WKing:
	Mov ecx, 1 ; 1 stands for white this will be used later
	Jmp @@King
	
	@@BKing:
	Mov ecx, 0 ; 0 stands for black this will be used later
	Jmp @@King

	@@King:
	Push ebx
	Push eax
	Cmp eax, ebx ; if the current < next you are going down
	Jl @@KingDown
	Jg @@KingUp
	jmp @@wrongmove


	@@KingDown:
	mov edx, 0
	sub eax, ebx
	neg eax
	mov ebx, JUMP1
	Div ebx
	Cmp edx, 0
	Je @@King_Down_Left  ;this means that the king is moving down left
	mov edx, 0
	Pop eax ;this will put eax back to the current
	pop ebx
	push ebx
	Push eax ;this will put eax back on the stack on top of ebx (next)	
    sub eax, ebx ; next-current
	neg eax
	mov ebx, JUMP2
	Div ebx
	Cmp edx, 0
	Je @@King_Down_Right ;this means that the king is moving down right
	Jmp @@wrongmove ;if the king is not going down right or left the move is ilegal

	@@KingUp:
	mov edx, 0
	sub eax, ebx
	mov ebx, JUMP1
	Div ebx
	Cmp edx, 0
	Je @@King_Up_Right  ;this means that the king is moving down left
	mov edx, 0
	Pop eax ;this will put eax back to the current
	pop ebx
	push ebx
	Push eax ;this will put eax back on the stack on top of ebx (next)	
    sub eax, ebx ; current - next 
	mov ebx, JUMP2
	Div ebx
	Cmp edx, 0
	Je @@King_Up_Left ;this means that the king is moving up left
	Jmp @@wrongmove ;if the king is not going up right or left the move is illegal

	@@King_Down_Right:
	Pop eax ;eax = current
	Pop ebx ;ebx = next stack is now empty
	Mov edx, JUMP2
	Push edx
	Jmp @@King_diagonalcheck


	@@King_Down_Left:
	Pop eax
	Pop ebx
	Mov edx, JUMP1
	Push edx
	Jmp @@King_diagonalcheck

	@@King_Up_Right:
	Pop eax
	Pop ebx
	Mov edx, JUMP1
	Neg edx
	Push edx
	Jmp @@King_diagonalcheck

	@@King_Up_Left:
	Pop eax
	Pop ebx
	mov edx, JUMP2
	Neg edx
	Push edx
	jmp @@King_diagonalcheck

	@@King_diagonalcheck:
	Pop edx ;edx = 36, -36, 28, -28 depending on direction
	Push edx ; now the stack only has edx
	Add eax, edx ;this makes a move in the direction we are going
	Cmp eax, ebx ;Are we at the destination ebx = next
	Je @@collision ; if we are at the end check if there is something there
	Mov edx, X 
	Cmp [eax], edx ;is the next a empty spot if it is we need to loop
	Je @@King_diagonalcheck
	Cmp ecx, 1 ;this checks which color of king we are moving 1 = white 0 = black
	Je @@WK_EAT
	Jmp @@BK_EAT

	@@WK_EAT:
	Mov edx, W
	Cmp [eax], edx ;is the next a white piece
	Je @@wrongmove ;this means that a white king is trying to eat white pawn which ;is not allowed
	mov edx, WK
	cmp [eax], edx
	je @@wrongmove
	Jmp @@King_diagonalcheck_has_eaten ;if he is not eating a white pawn he has to be eating black pawn
	
	@@BK_EAT:
	Mov edx, B
	Cmp [eax], edx ;is the next a black piece
	Je @@wrongmove ;this means that a black king is trying to eat black pawn which ;is not allowed
	mov edx, BK
	cmp [eax], edx
	je @@wrongmove
	Jmp @@King_diagonalcheck_has_eaten 

	@@collision:
	Mov edx, X
	Cmp [eax], edx 
	Je @@goodmove
	jmp @@wrongmove
	
	@@collisionKill:
	Mov edx, X
	Cmp [eax], edx 
	Je @@KingKill
	jmp @@wrongmove
	
	@@KingKill:
	cmp ecx, 1
	je @@BkingKill
	dec [white]
	jmp @@goodmove
	
	@@BkingKill:
	dec [black]
	jmp @@goodmove
	
	
	@@King_diagonalcheck_has_eaten:
	Pop edx
	Push edx
	Add eax, edx
	Cmp eax, ebx ;Are we at the destination
	Je @@collisionKill
	Mov edx, X 
	Cmp [eax], edx ;is the next a empty spot if it is we need to check for the next  
	Je @@King_diagonalcheck_has_eaten
	jmp @@wrongmove ; if the king already ate and he sees something else then a X on 
	                ;his way to next, then it’s a wrong move since king can only eat 1 piece at a time 

	@@whitecheck:
    push eax
    add eax, JUMP1
    push eax
    add eax, JUMP1
    mov edx, eax
    pop eax
    cmp eax, ebx
    je @@checkempty
    cmp edx, ebx
    push edx
    je @@eatw
    pop edx
    pop eax
    add eax, JUMP2
    push eax
    add eax, JUMP2
    mov edx, eax
    pop eax
    cmp eax, ebx
    je @@checkempty
    cmp edx, ebx
    push edx
    je @@eatw
    jne @@wrongmove

    @@blackcheck:
    push eax
    sub eax, JUMP1
    push eax
    sub eax, JUMP1 ;this can lead to errors if this is outside index
    mov edx, eax
    pop eax
    cmp eax, ebx
    je @@checkempty
    cmp edx, ebx
    push edx
    je @@eatb
    pop edx
    pop eax
    sub eax, JUMP2
    push eax
    sub eax, JUMP2
    mov edx, eax
    pop eax
    cmp eax, ebx
    je @@checkempty
    cmp edx, ebx
    push edx
    je @@eatb
    jmp @@wrongmove

    @@checkempty:
	cmp [onlyEat], 1
	je @@wrongmove
    mov edx, X
    cmp [eax], edx
    je @@goodmove
    jmp @@wrongmove
	
    @@eatw:
    mov edx, W
    cmp [eax], edx
    je far @@wrongmove
	mov edx, X
    cmp [eax], edx
    je @@wrongmove
	mov edx, WK
    cmp [eax], edx
    je far @@wrongmove
	mov esi, 1
    jmp @@caneat
	

    @@eatb:
    mov edx, B
    cmp [eax], edx
    je far @@wrongmove
	mov edx, X
    cmp [eax], edx
    je @@wrongmove
	mov edx, BK
    cmp [eax], edx
    je @@wrongmove
	mov esi, 0
    jmp @@caneat

    @@caneat:
    pop eax
    mov edx, X
    cmp [eax], edx
    je @@kill
    jmp @@wrongmove
	
	@@kill:
	cmp [mustEat], 1
	je @@MustEat
	cmp esi, 1
	je @@killBlack
	dec [white]
	jmp @@goodmove
	
	@@killBlack:
	dec [black]
	jmp @@goodmove
	
	@@MustEat:
	mov ax, 1
	jmp @@End

    @@goodmove:
    mov ax, TRUE
    jmp @@End

    @@wrongmove:
    mov ax, FALSE

    @@End:
	mov [onlyEat], 0
    ret
ENDP CheckMove

; MakeMove takes a pointer to the board, a current position and a next position
; it will make the move from current to next and put empty squares on his way
PROC MakeMove
    ARG @@arrayptr:dword, @@current:dword, @@next:dword
    USES EAX, EBX, ECX, EDX
	mov [jumpCtr], 0
	mov edx, [@@arrayptr]
	mov ebx, [@@current]
	add ebx, edx  ;ebx: current position on the board
	push ebx
	mov eax, [ebx]
	mov ebx, edx
	mov edx, [@@next]
	add ebx, edx  ;ebx: next position on the board
	xchg eax, [ebx] ;make the move
	cmp edx, FIRST_ROW   ;check if you move to first row
	jle @@CheckKing
	cmp edx, LAST_ROW    ;check if you move to last row
	jg @@CheckKing       
	jmp @@Continue
	
	@@CheckKing:  ;check which king it will be
	mov edx, W
	cmp [ebx], edx
	je @@WhiteKing
	mov edx, BK
	jmp @@MakeKing
	
	@@WhiteKing:
	mov edx, WK
	
	@@MakeKing:
	mov [ebx], edx

	@@Continue:
    mov eax, [@@current]
    sub eax, [@@next]   ;check if we are going up or down
	cmp eax, 0
    mov edx, 0
    mov ebx, JUMP1
    jl @@MoveDown
    jmp @@UP

    @@MoveDown:
    neg eax
    Jmp @@DOWN

    @@UP:
    div ebx
    mov ecx, eax
    pop eax
    push eax
    cmp edx, 0
	mov ebx, 0
    je @@JumpUpRight
    jmp @@JumpUpLeft

    @@DOWN:
    div ebx
    mov ecx, eax
    pop eax
    push eax
    cmp edx, 0
	mov ebx, 1
    je @@JumpDownLeft
    jmp @@JumpDownRight

    @@JumpUpLeft:
    mov edx, JUMP2
    jmp @@MakeX

    @@JumpUpRight:
    mov edx, JUMP1
    jmp @@MakeX

    @@JumpDownLeft:
    mov edx, JUMP1
    jmp @@MakeX

    @@JumpDownRight:
    mov edx, JUMP2
	jmp @@MakeX
	
		@@AddOrSub:
		pop ebx
		cmp ebx, 1
		je @@Add
		jmp @@Sub
		
		@@Add:
		add eax, edx
		jmp @@MakeX
		
		@@Sub:
		sub eax, edx
		
		@@MakeX:          ;wanneer je een sprong maakt zal het alle element op de diagonaal tussen
		dec ecx           ;huidige en volgende positie vervangen door X
		inc [jumpCtr]
		push ebx        
		mov ebx, X
		mov [eax], ebx
		jcxz @@End
		jmp @@AddOrSub

    @@End:
    ret
ENDP MakeMove

; CheckAnotherEat will check for a piece on position @@position, 
; if it can eat something or not (ax TRUE or FALSE)
PROC CheckAnotherEat
	ARG @@arrayptr: dword, @@position: dword
	USES ebx, ecx, edx
	mov [mustEat], 1    
	mov ebx, [@@position]
	add ebx, JUMP1
	add ebx, JUMP1
	call CheckMove, [@@arrayptr], [@@position], ebx
	cmp ax, 1
	je @@AnotherEat
	mov ebx, [@@position]
	add ebx, JUMP2
	add ebx, JUMP2
	call CheckMove, [@@arrayptr], [@@position], ebx
	cmp ax, 1
	je @@AnotherEat
	mov ebx, [@@position]
	sub ebx, JUMP1
	sub ebx, JUMP1
	call CheckMove, [@@arrayptr], [@@position], ebx
	cmp ax, 1
	je @@AnotherEat
	mov ebx, [@@position]
	sub ebx, JUMP2
	sub ebx, JUMP2
	call CheckMove, [@@arrayptr], [@@position], ebx
	cmp ax, 1
	je @@AnotherEat
	jmp @@NoEat
	
	@@AnotherEat:
	mov ax, 1
	jmp @@end
	
	@@NoEat:
	mov ax, 0
	
	@@end:
	mov [mustEat], 0
	ret
ENDP CheckAnotherEat

; CheckEatAll will loop trough the 2d array and call CheckAnotherEat on
; every piece and if comes by a piece that can eat it will save it's position
PROC CheckEatAll
	ARG @@arrayptr: dword
	uses eax, ebx, ecx, edx, esi, edi
	
	mov ebx, 0
	mov [musteatPos1], -1
	mov [musteatPos2], -1
	mov ecx, 64
	
	@@Loop:
	cmp ecx, 0
	je @@end
	call CheckAnotherEat, [@@arrayptr], ebx
	cmp ax, 1
	jne @@next
	
	cmp [musteatPos1], -1
	jne @@add2
	mov [musteatPos1], ebx
	jmp @@next
	
	@@add2: ;this branch doesn't work, we didn't understand why
	mov [musteatPos2], ebx
	jmp @@end
	
	@@next: ; go to the next piece
	add ebx, 4
	dec ecx
	jmp @@Loop
	
	@@end:
	ret
ENDP CheckEatAll

;------------------------------------------------------------------------------------
;DRAW PROCEDURES
;------------------------------------------------------------------------------------
;Set the video mode
PROC setVideoMode
	ARG 	@@VM:byte
	USES 	eax

	movzx ax,[@@VM]
	int 10h

	ret
ENDP setVideoMode

; Update the colour palette.
; 	* Ncolours: number of colours that have to be updated [word]
PROC updateColourPalette
	ARG	 	@@Psource:dword, @@Nbytes: dword, @@Firstcolor: word
	USES 	eax, ecx, edx, esi

	mov esi, [@@Psource]
	mov ecx, [@@Nbytes] ; amount of color bytes to read (movzx = zero extend)
	

	mov dx, 03C8h 		; DAC write port
	mov ax, [@@Firstcolor]	; index of first color to change
	out dx, al			; write to IO
	inc dx
	rep outsb			; update colors

	ret
ENDP updateColourPalette

; Wait for a specific keystroke.
PROC waitForSpecificKeystroke
	ARG 	@@key:byte
	USES 	eax

	@@waitForKeystroke:
		mov	ah,00h
		int	16h
		cmp	al,[@@key]
	jne	@@waitForKeystroke

	ret
ENDP waitForSpecificKeystroke

; Terminate the program.
PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

; DrawImage takes a filepath pointer, a row and column (so that it knows where
; where to draw the image), width and height of the image, and the place in data
PROC drawImage
	ARG	 @@filepathptr: dword, @@row: dword, @@col: dword, @@width: dword, @@height: dword, @@data: dword
	USES eax, ebx, ecx, edx, esi, edi 
	
	; open file, get filehandle in AX
	mov al, 0 ; read only
	mov edx, [@@filepathptr]
	mov ah, 3dh
	int 21h
	
	mov  edx, offset openErrorMsg
	jc @@print_error ; carry flag is set if error occurs

	; read file data
	mov bx, ax ; move filehandle to bx
	mov ecx, FULLPALLETESIZE
	push eax
	push ebx
	mov eax, [@@width]
	mov ebx, [@@height]
	mul ebx
	add ecx, eax
	pop ebx
	pop eax
	mov edx, offset palette
	mov ah, 3fh
	int 21h

	mov  edx, offset readErrorMsg
	jc @@print_error
	
	; close file
	mov ah, 3Eh
	int 21h
	
	mov  edx, offset closeErrorMsg
	jc @@print_error
	
	; copy to video buffer, update palette
	; place the image on the good position
	mov esi, [@@data]
	mov edi, VMEMADR
	push eax
	push ebx
	mov eax, [@@row]
	mov ebx, SCRWIDTH
	mul ebx
	mov ebx, SQRHEIGHT
	mul ebx
	push eax
	mov eax, [@@col]
	mov ebx, SQRWIDTH
	mul ebx
	pop ebx
	add eax, ebx
	add edi, eax
	pop ebx
	pop eax
	
	mov edx, [@@height]
	@@Loop:
	cmp edx, 0
	je @@end
	mov ecx, [@@width]
	rep movsb
	dec edx
	add edi, SCRWIDTH
	sub edi, [@@width]
	jmp @@Loop
	
	@@end:
	call updateColourPalette, offset palette, FULLPALLETESIZE, 0
	ret

@@print_error:
	call setVideoMode, 03h
	mov ah, 09h
	int 21h
	
	mov	ah, 00h
	int	16h
	call terminateProcess	

ENDP drawImage

; DrawPieces will loop trough the matrix and draw all the pieces on the
; correct position. It will also highlight the piece that must move.
PROC drawPieces
	ARG @@arrayptr: dword
	USES EAX, EBX, ECX, EDX, esi, edi
	mov ebx, 0 ;row
	mov ecx, 0 ;column
	mov eax, [@@arrayptr] ;first element of board
	mov esi, [@@arrayptr]
	add esi, [musteatPos1]
	mov edi, [@@arrayptr]
	add edi, [musteatPos2]
	
	@@Loop:
	cmp ebx, ROWS ;end of rows
	je @@End
	
	cmp eax, esi
	je @@CheckMust
	cmp eax, edi
	je @@CheckMust
	mov edx, W
	cmp [eax], edx
	je @@White
	mov edx, B
	cmp [eax], edx
	je @@Black
	mov edx, BK
	cmp [eax], edx
	je @@BlackKing
	mov edx, WK
	cmp [eax], edx
	je @@WhiteKing
	mov edx, X
	cmp [eax], edx
	je @@Empty
	
	@@Next:        
	inc ecx
	add eax, 4
	
	cmp ecx, COLUMNS
	je @@NextLine
	jmp @@Loop
	
	@@Empty:
	mov edx, offset empty_file
	jmp @@Draw
	
	@@White:
	mov edx, offset white_file
	jmp @@Draw
	
	@@Black:
	mov edx, offset black_file
	jmp @@Draw
	
	@@BlackKing:
	mov edx, offset bking_file
	jmp @@Draw
	
	@@WhiteKing:
	mov edx, offset wking_file
	jmp @@Draw
	
	@@Draw:
	call drawImage, edx, ebx, ecx, SQRWIDTH, SQRHEIGHT, offset imagedata1
	jmp @@Next
	
	@@NextLine:
	inc ebx
	mov ecx, 0
	jmp @@Loop
	
	@@CheckMust:
	mov edx, W
	cmp [eax], edx
	je @@drawMustWhite
	mov edx, WK
	cmp [eax], edx
	je @@drawMustWhiteKing
	mov edx, BK
	cmp [eax], edx
	je @@drawMustBlackKing
	mov edx, offset selectb_file
	jmp @@Draw
	
	@@drawMustWhite:
	mov edx, offset selectw_file
	jmp @@Draw
	
	@@drawMustWhiteKing:
	mov edx, offset selectwk_file
	jmp @@Draw
	
	@@drawMustBlackKing:
	mov edx, offset selectbk_file
	jmp @@Draw

	
	@@End:
	ret
ENDP drawPieces


PROC MousePosition
	@@mainLoop:
	mov ax, 3
	int 33h
	test bx, 1
	jz @@mainLoop
	sar cx,1
	jmp @@CalcPos
	
	; calculate the position of the mouse to the board
	@@CalcPos:
	push dx
	mov edx, 0
	movzx eax, cx
	mov ebx, 40
	div ebx
	push eax
	mov ebx, 4
	mul ebx
	mov [currentPos], eax
	
	pop eax
	pop dx
	push eax
	movzx eax, dx
	mov edx, 0
	mov ebx, 25
	div ebx
	push eax
	mov ebx, 32
	mul ebx
	mov ebx, [currentPos]
	add eax, ebx
	mov [currentPos], eax
	
	@@DrawPos:
	mov ebx, offset board
	add ebx, eax
	mov edx, Z
	cmp [ebx], edx
	je @@drawNothing
	mov edx, W
	cmp [ebx], edx
	je @@drawWhite
	mov edx, B
	cmp [ebx], edx
	je @@drawBlack
	mov edx, BK
	cmp [ebx], edx
	je @@drawBlackKing
	mov edx, WK
	cmp [ebx], edx
	je @@drawWhiteKing
	mov edx, X
	cmp [ebx], edx
	je @@drawEmpty
	jmp @@end
	
	@@drawWhite:
	mov edx, offset selectw_file
	jmp @@draw
	
	@@drawBlack:
	mov edx, offset selectb_file
	jmp @@draw
	
	@@drawBlackKing:
	mov edx, offset selectbk_file
	jmp @@draw
	
	@@drawWhiteKing:
	mov edx, offset selectwk_file
	jmp @@draw
	
	@@drawEmpty:
	mov edx, offset selecte_file
	jmp @@draw
	
	@@drawNothing:
	mov edx, 0
	jmp @@draw
	
	@@draw:
	pop ebx
	pop ecx
	mov ax, 2
	int 33h
	cmp edx, 0
	je @@end
	call drawImage, edx, ebx, ecx, SQRWIDTH, SQRHEIGHT, offset imagedata1
	jmp @@end
	
	@@end:
	mov ax, 1
	int 33h
	ret
ENDP MousePosition
	
PROC Delay        ;delay function
	mov cx, 0  
	mov dx, 65000
	mov ah, 86h
	int 15h
	ret
ENDP Delay

PROC CheckEnd
mov ax, 2
	int 33h
	cmp [white], 0
	je @@BlackWon
	cmp [black], 0
	je @@WhiteWon
	cmp [white], 1
	je @@CheckNoWinner
	jmp @@Continue
	
	@@BlackWon:
	call drawImage, offset blackwin, 0, 0, SCRWIDTH, SCRHEIGHT, offset imagedata1
	jmp @@Won
	
	@@WhiteWon:
	call drawImage, offset whitewin, 0, 0, SCRWIDTH, SCRHEIGHT, offset imagedata1
	jmp @@Won
	
	@@Won:
	mov ax, TRUE
	jmp @@End
	
	@@CheckNoWinner:
	cmp [black], 1
	jne @@BlackWon
	je @@NoWinner
	
	@@Continue:
	mov ax, 1
	int 33h
	mov ax, FALSE
	jmp @@End
	
	@@NoWinner:
	call drawImage, offset nowin, 0, 0, SCRWIDTH, SCRHEIGHT, offset imagedata1
	jmp @@Won
	
	@@End:
	ret
ENDP CheckEnd

PROC StartGame
	sti                                 ; Set The Interrupt Flag
    cld 	; Clear The Direction Flag
	push ds
	pop	es
	call setVideoMode,13h
	mov ax, 0
	int 33h
	call drawImage, offset start_file, 0, 0, SCRWIDTH, SCRHEIGHT, offset imagedata1
	call waitForSpecificKeystroke, 0Dh ; keycode for ENTER
	call drawImage, offset board_file, 0, 0, SCRWIDTH, SCRHEIGHT, offset imagedata1
	ret
ENDP StartGame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC main
    call StartGame

	@@GameLoop:
	call CheckEatAll, offset board
	call drawPieces, offset board
	mov ax, 1
	int 33h
	call CheckEnd
	cmp ax, 0
	jne @@End
	call MousePosition
	mov esi, [currentPos]
	mov [oldPos], esi
	call Delay
	call Delay
	call MousePosition
	call CheckMove, offset board, [oldPos], [currentPos]
	cmp ax, TRUE
	jne @@GameLoop
	call MakeMove, offset board, [oldPos], [currentPos]
	call CheckEatAll, offset board
	cmp [musteatPos1], NO_POSITION
	je @@Switch
	cmp [jumpCtr], 1
	jg @@GameLoop
	@@Switch:
	mov edx, [currentTurn]
	neg edx
	mov [currentTurn], edx
	jmp @@GameLoop
	
	@@End:
	call waitForSpecificKeystroke, 001Bh ; keycode for ESC
	call terminateProcess
	ret
ENDP main
;-----------------------------------------------
DATASEG
;variables:
arrlen dd 64
board dd W, Z, W, Z, W, Z, W, Z
	  dd Z, W, Z, W, Z, W, Z, W
	  dd W, Z, W, Z, W, Z, W, Z
      dd Z, X, Z, X, Z, X, Z, X 
	  dd X, Z, X, Z, X, Z, X, Z 
	  dd Z, B, Z, B, Z, B, Z, B
      dd B, Z, B, Z, B, Z, B, Z
	  dd Z, B, Z, B, Z, B, Z, B
musteatPos1 dd NO_POSITION
musteatPos2 dd NO_POSITION
mustposx dd 0
mustposy dd 0
black dd 12
white dd 12
mustEat dd 0
onlyEat dd 0
currentTurn dd 1
oldPos dd 0      
jumpCtr dd 0
currentPos dd 0
;;images:
;pieces
black_file db "black.bin", 0
white_file db "white.bin", 0
empty_file db "empty.bin", 0
bking_file db "bking.bin", 0
wking_file db "wking.bin", 0
;selected pieces
selectb_file db "selectb.bin", 0
selectw_file db "selectw.bin", 0
selecte_file db "selecte.bin", 0
selectbk_file db "selectbk.bin", 0
selectwk_file db "selectwk.bin", 0
;screens
board_file db "board.bin", 0	
start_file db "start.bin", 0
nowin db "nowin.bin", 0
whitewin db "whitewin.bin", 0
blackwin db "blackwin.bin", 0
;messages:
openErrorMsg db "could not open file", 13, 10, '$'
readErrorMsg db "could not read data", 13, 10, '$'
closeErrorMsg db "error during file closing", 13, 10, '$'
;-----------------------------------------------
UDATASEG
palette db FULLPALLETESIZE dup (?)
imagedata1 db PIXELCOUNT1 dup (?)
imagedata2 db PIXELCOUNT2 dup (?)
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main

