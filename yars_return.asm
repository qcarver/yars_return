	processor 6502
	include "vcs.h"
	include "macro.h"
	SEG.U VARS
	ORG $80

YarXPos	byte	; Positions
YarYPos byte	;
SwirlXPos byte  ;
SwirlYPos byte  ;


	SEG CODE
	ORG $F000
Start:
	CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #10
	sta YarYPos
	lda #60
	sta YarXPos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to LUTs 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #<YarSprite
	sta YarSpritePtr	;lo-byte for address of Yar sprite
	lda #>YarSprite
	sta YarSpritePtr+1	;hi-byte for address of Yar sprite	

	lda #<YarColor
	sta YarColorPtr	;lo-byte for address of Yar sprite
	lda #>YarColor
	sta YarColorPtr+1	;hi-byte for address of Yar sprite	

	lda #<SwirlSprite
	sta SwirlSpritePtr	;lo-byte for address of Swirl sprite
	lda #>SwirlSprite
	sta SwirlSpritePtr+1	;hi-byte for address of Swirl sprite	

	lda #<SwirlColor
	sta SwirlColorPtr	;lo-byte for address of Swirl sprite
	lda #>SwirlColor
	sta SwirlColorPtr+1	;hi-byte for address of Swirl sprite	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Repeating playfield is default with clean start
;	lda #1
;	sta CTRLPF	; Mirrored playfield

StartFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VSYNCH and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda INTIM
	sta VBLANK
	sta VSYNC
	REPEAT 3
		sta WSYNC
	REPEND
	lda #0
	sta VSYNC
	REPEAT 37
		sta WSYNC
	REPEND
	sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visible Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PlayfieldLoop
 
	ldx #192				; use x as playfield line counter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visible Row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.GameLineLoop
	ldy #$A0		; make sky...	
	sty COLUBK		; 	bg color blue
	lda PFColors,x		; get color...
	sta COLUPF		; 	for row
	lda PF0DataA,x		; get bit..
	sta PF0			; 	patterns
	lda PF1DataA,x		; 	for the
	sta PF1			; 	three 
	lda PF2DataA,x		; 	playfield
	sta PF2			; 	vars
	sta WSYNC
	dex			; x--
	bne .GameLineLoop
	
	;;something glitchy at top, make it blend in
	ldy #$A0		; make sky...	
	sty COLUBK		; bg color blue 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK				; turn on VBLANK again
	REPEAT 30
		sta WSYNC			; display 30 recommended lines of VBlank Overscan
	REPEND
	lda #0
	sta VBLANK				; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jmp StartFrame			; continue to display the next frame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Graphic and Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

YarSprites:
Yar_0
	.byte $00 ; |		|
	.byte $0E ; |	XXX |
	.byte $08 ; |	X	|
	.byte $8D ; |X	XX X|
	.byte $F2 ; |XXXX  X |
	.byte $F2 ; |XXXX  X |
	.byte $8D ; |X	XX X|
	.byte $08 ; |	X	|
	.byte $0E ; |	XXX |
Yar_1  
	.byte $00 ; |		|
	.byte $40 ; | X	  |
	.byte $70 ; | XXX	|
	.byte $9D ; |X  XXX X|
	.byte $F2 ; |XXXX  X |
	.byte $F2 ; |XXXX  X |
	.byte $9D ; |X  XXX X|
	.byte $70 ; | XXX	|
	.byte $40 ; | X	  |


SwirlSprites
Swirl_0	
	.byte $00 ; |........|
	.byte $60 ; |.XX.....|
	.byte $11 ; |...X...X|
	.byte $09 ; |....X..X|
	.byte $3A ; |..XXX.X.|
	.byte $5C ; |.X.XXX..|
	.byte $90 ; |X..X....|
	.byte $88 ; |X...X...|
	.byte $06 ; |.....XX.|
Swirl_1	
	.byte $00 ; |........|
	.byte $04 ; |.....X..|
	.byte $62 ; |.XX...X.|
	.byte $92 ; |X..X..X.|
	.byte $1C ; |...XXX..|
	.byte $38 ; |..XXX...|
	.byte $49 ; |.X..X..X|
	.byte $46 ; |.X...XX.|
	.byte $20 ; |..X.....|
Swirl_2	
	.byte $00 ; |........|
	.byte $18 ; |...XX...|
	.byte $06 ; |.....XX.|
	.byte $64 ; |.XX..X..|
	.byte $99 ; |X..XX..X|
	.byte $99 ; |X..XX..X|
	.byte $26 ; |..X..XX.|
	.byte $20 ; |..X.....|
	.byte $18 ; |...XX...|


YarColor:
	byte #$00
	byte #$0E
	byte #$0E
	byte #$0E
	byte #$0E
	byte #$0E
	byte #$0E	
	byte #$0E
	byte #$0E
SwirlColor:
	byte #$00
	byte #$46
	byte #$46
	byte #$46
	byte #$46
	byte #$46
	byte #$46
	byte #$46
	byte #$46


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Playfield Graphics and Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PF0DataA
	.byte %11110000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11010000
	.byte %11010000
	.byte %11010000
	.byte %11010000
	.byte %11010000
	.byte %11010000
	.byte %10010000
	.byte %10010000
	.byte %10010000
	.byte %10010000
	.byte %11010000
	.byte %11010000
	.byte %11000000
	.byte %11000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %10000000
	.byte %00000000
	.byte %10000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00100000
	.byte %00010000
	.byte %00010000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11000000
	.byte %11100000
	.byte %11100000
	.byte %10110000
	.byte %11010000
	.byte %11110000
	.byte %01110000
	.byte %11110000
	.byte %11110000
	.byte %00110000
	.byte %00110000
	.byte %00110000
	.byte %00000000
	.byte %00010000
	.byte %00010000
	.byte %00000000
	.byte %00000000

PF1DataA
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %10101000
	.byte %10101010
	.byte %10101010
	.byte %10111010
	.byte %00101110
	.byte %10101000
	.byte %00101010
	.byte %10111010
	.byte %00101110
	.byte %00101010
	.byte %10111000
	.byte %10100010
	.byte %10110111
	.byte %10110011
	.byte %10110110
	.byte %10110110
	.byte %10100010
	.byte %10100010
	.byte %10100010
	.byte %10100011
	.byte %00100011
	.byte %00100011
	.byte %00100010
	.byte %00100010
	.byte %00000010
	.byte %00100010
	.byte %00100000
	.byte %00000000
	.byte %10100000
	.byte %10000010
	.byte %00100010
	.byte %10100010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111110
	.byte %00111110
	.byte %00011100
	.byte %00011100
	.byte %00011100
	.byte %00000001
	.byte %01000011
	.byte %11100111
	.byte %11101111
	.byte %11101111
	.byte %11101111
	.byte %11100111
	.byte %01000111
	.byte %00000111
	.byte %00000011
	.byte %01110001
	.byte %11111000
	.byte %11111000
	.byte %11011000
	.byte %01110000
	.byte %00000000
	.byte %00000011
	.byte %00000001

PF2DataA
	.byte %00000111
	.byte %00000111
	.byte %00000000
	.byte %00000111
	.byte %00000111
	.byte %00000111
	.byte %00000111
	.byte %00000101
	.byte %00000101
	.byte %00000101
	.byte %00000101
	.byte %00000100
	.byte %00000100
	.byte %00000101
	.byte %00000101
	.byte %00000101
	.byte %00000101
	.byte %10001101
	.byte %10001101
	.byte %10001001
	.byte %10001001
	.byte %10001001
	.byte %11011001
	.byte %11011001
	.byte %01010001
	.byte %01010001
	.byte %01110001
	.byte %01110000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11000000
	.byte %11000000
	.byte %10000000
	.byte %10000000
	.byte %00011111
	.byte %00111111
	.byte %01111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111110
	.byte %11111011
	.byte %11111101
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11101111
	.byte %10111110
	.byte %11111100
	.byte %11011100
	.byte %11111000
	.byte %11111001
	.byte %01110000

PFColors
	.byte $1E	;; far as I can tell this color is never used
	.byte $A0	;; this color wraps from a glitch at the bottom
	.byte $06
	.byte $06
	.byte $06
	.byte $0E
	.byte $06
	.byte $06
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $40
	.byte $40
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $06
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $40
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E


	ECHO ([$FFFC-.]d), "bytes free"

	org $fffc
	.word Start
	.word Start

