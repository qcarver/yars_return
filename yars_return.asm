	processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include "vcs.h"
	include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg.u VARS
	org $80

YarXPos		byte	; Positions
YarYPos 	byte	;
QuotileXPos 	byte    ;
QuotileYPos 	byte	;
YarSpritePtr    word	; Pointers
YarColorPtr     word	;
QuotileSpritePtr	word	;
QuotileColorPtr	word	;
scratch		byte    ;
YarAnimOffset   byte    ;
Random		byte	;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
YAR_HEIGHT equ 	9	; Height of Yar Sprite
QUOTILE_HEIGHT equ 9	; Height of Quotile
YarN equ 9
YarS equ 0
YarW equ 18
YarNW equ 27
YarSW equ 36

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        seg Code	
	org $F000

Reset:
	CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #60
	sta YarYPos
	lda #80
	sta YarXPos
	lda #52
	sta QuotileXPos 
	lda #8
	sta QuotileYPos
	lda #69
	sta Random

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

	lda #<QuotileSprite
	sta QuotileSpritePtr	;lo-byte for address of Quotile sprite
	lda #>QuotileSprite
	sta QuotileSpritePtr+1	;hi-byte for address of Quotile sprite	

	lda #<QuotileColor
	sta QuotileColorPtr	;lo-byte for address of Quotile sprite
	lda #>QuotileColor
	sta QuotileColorPtr+1	;hi-byte for address of Quotile sprite	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBlank 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda YarXPos		; this will be the A parameter for the subr
	ldy #0			; this will be the object type paramter 
	jsr SetObjectXPos	; set Yar horizontal position, call subroutine

	lda QuotileXPos		; same as above, but Quotile instead of Yar
	ldy #1
	jsr SetObjectXPos	; set player1 horizontal position

	sta WSYNC
	sta HMOVE		; apply the offsets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
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
;; Visible Frame: display 192 scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine
	ldy #$A0		; make sky...	
	sty COLUBK		; bg color blue 
 
	ldx #96		; row cnt:192:1,96:2line kernel 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visible Row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.GameLineLoop:
        ;;BEGIN: Draw Playfield______________
	lda PFColors,x		; get color...
	sta COLUPF		; 	for row
	lda PF0DataA,x		; get bit..
	sta PF0			; 	patterns
	lda PF1DataA,x		; 	for the
	sta PF1			; 	three 
	lda PF2DataA,x		; 	playfield
	sta PF2			; 	vars
	sta WSYNC
	;;END: Draw Playfield________________

.AreWeInsideYarSprite:
	txa		; transfer linePos to A
	sec		; sec (before subtract)
	sbc YarYPos	; SpriteY - linepos
	cmp YAR_HEIGHT	; w/in height of sprite?
	bcc .DrawYarSprite
	lda #0		; else: set  empty sprite line

.DrawYarSprite        
	clc		;add
	adc YarAnimOffset	; Yar animation offset
        tay
	lda (YarSpritePtr),y	;ld Sprite at offset
	;;sta WSYNC	; wait for scanline
	sta GRP0	; set graphics for Yar
	lda #$0E	;load color
	sta COLUP0	; set color of Yar

.AreWeInsideQuotileSprite:
	txa		; transfer linePos to A
	sec		; sec (before subtract)
	sbc QuotileYPos	; SpriteY - linepos
	cmp QUOTILE_HEIGHT	; w/in height of sprite?
	bcc .DrawQuotileSprite
	lda #0		; else: set  empty sprite line

.DrawQuotileSprite
	tay		; hint: Y only indirect register
	lda #5		; b101 stretches sprite Horizon
	sta NUSIZ1
	lda (QuotileSpritePtr),y	;ld Sprite at offset
	sta WSYNC	; wait for scanline
	sta GRP1	; set graphics for Quotile
	lda (QuotileColorPtr),y	;load color
	sta COLUP1	; set color of Quotile

	dex			; x--
	bne .GameLineLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK				; turn on VBLANK again
	REPEAT 30
		sta WSYNC			; 30 blank lines of overscan 
	REPEND
	lda #0
	sta VBLANK				; turn off VBLANK


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckP0Up:
	lda #%00010000	; player0 joystick up
	bit SWCHA	
	bne CheckP0Down	; If bit pattern doesn't bypass UP
        inc YarYPos	
        lda YarN	;	set Yar glpyh
        sta YarAnimOffset ;		to North Facing
CheckP0Down:
	lda #%00100000   ; player0 joystick down
	bit SWCHA
	bne CheckP0Left  ; If bit pattern doesn't match, bypass Down Block
        dec YarYPos	
        lda YarS	;	set Yar glpyh
        sta YarAnimOffset ;		to South Facing
CheckP0Left:
	lda #%01000000   ; player0 joystick left
	bit SWCHA
	bne CheckP0Right  ; If bit pattern doesn't match, bypass Left Block
        dec YarXPos	
        lda YarW	;	set Yar glpyh
        sta YarAnimOffset ;		to North Facing
        lda 0		  ; reflect		
        STA REFP0	  ; 	player 0
CheckP0Right:
	lda #%10000000   ; player0 joystick right
	bit SWCHA
	bne EndInputCheck  ; If bit pattern doesn't match, bypass Right Block
        inc YarXPos
        lda YarW	;	set Yar glpyh
        sta YarAnimOffset ;		to North Facing
        lda 8		  ; reflect		
        STA REFP0	  ; 	player 0
EndInputCheck

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update Quotile position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda QuotileYPos
        clc
        cmp #96
        bmi IncrementQuotile	;; if QuotleYPos < 96
        lda 0			;; else load 0	
        sta QuotileYPos		;;        into QuotileYPos
	jsr GetRndQPos		;; jump Quotile to a new Horizontal location 	
IncrementQuotile
	inc QuotileYPos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jmp StartFrame			; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position w/ fine offset 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our target
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missle1, 4:ball) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
	sta WSYNC		; start a fresh new scanline
	sec			; set the carry-flag before subtracting
.Div15Loop			; divide is just continuos subtraction
	sbc #15			; subtract 15 from A in accumulator
	bcs .Div15Loop		; loop until carry-flag is clear
	eor #7			; xor signs corse remainder from -8 to 7
	asl
	asl
	asl
	asl		; move four lowest bits to hi (littleendian)
	sta HMP0,Y		; store the fine offset to the correct HMxx
	sta RESP0,Y		; fix object position in 15 step increment
	rts			; return	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to put a random number in Quotile X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a random number using a Linear-Feedback Shift Register  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRndQPos subroutine
	lda Random	; Load starting random seed
	asl 		; <<1
	eor Random	; xor
	asl		; <<1
	eor Random	; xor
	asl
	asl
	eor Random
	asl
	rol Random	; performs a series of shifts and bit operations
	sta QuotileXPos ; put the random number in Quotiles X coord
	rts 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Graphic and Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

YarSprite:
;Offset Legend
;JoystickPos 00010000 Heads N	GlyphOffset	16
;JoystickPos 00100000 Heads S	GlyphOffset	32
;JoystickPos 01000000 Heads W	GlyphOffset	64
;JoystickPos 01010000 Heads NW	GlyphOffset	80
;JoystickPos 01100000 Heads SW	GlyphOffset	96
;JoystickPos 10000000 Heads E	GlyphOffset	64	and reflect
;JoystickPos 10010000 Heads NE	GlyphOffset	80	and reflect
;JoystickPos 10100000 Heads SE	GlyphOffset	96	and reflect
;YarN
   .byte $00 ; |........|
   .byte $24 ; |..X..X..|
   .byte $18 ; |...XX...|
   .byte $24 ; |..X..X..|
   .byte $24 ; |..X..X..|
   .byte $7E ; |.XXXXXX.|
   .byte $5A ; |.X.XX.X.|
   .byte $DB ; |XX.XX.XX|
   .byte $3C ; |..XXXX..|
;YarS				+9
   .byte $00 ; |........|	
   .byte $3C ; |..XXXX..|
   .byte $DB ; |XX.XX.XX|
   .byte $5A ; |.X.XX.X.|
   .byte $7E ; |.XXXXXX.|
   .byte $24 ; |..X..X..|
   .byte $24 ; |..X..X..|
   .byte $18 ; |...XX...|
   .byte $24 ; |..X..X..|	
;YarW				+18
   	.byte $00 ; |........|
   	.byte $02 ; |......X.|
   	.byte $0E ; |....XXX.|
   	.byte $99 ; |X..XX..X|
   	.byte $67 ; |.XX..XXX|
   	.byte $67 ; |.XX..XXX|
   	.byte $99 ; |X..XX..X|
   	.byte $0E ; |....XXX.|
   	.byte $02 ; |......X.|
;YarNW				+27
           .byte $00 ; |........|
           .byte $20 ; |..X.....|
           .byte $30 ; |..XX....|
           .byte $ED ; |XXX.XX.X|
           .byte $47 ; |.X...XXX|
           .byte $2C ; |..X.XX..|
           .byte $3F ; |..XXXXXX|
           .byte $17 ; |...X.XXX|
           .byte $36 ; |..XX.XX.|
;YarSW   			+36
				
   .byte $36 ; |..XX.XX.|
   .byte $17 ; |...X.XXX|
   .byte $3F ; |..XXXXXX|
   .byte $2C ; |..X.XX..|
   .byte $47 ; |.X...XXX|
   .byte $ED ; |XXX.XX.X|
   .byte $30 ; |..XX....|
   .byte $20 ; |..X.....|
           .byte $00 ; |........|
        .byte $00 ; |        |
	.byte $24 ; |  X  X  |
	.byte $99 ; |X  XX  X|
	.byte $BD ; |X XXXX X|	
	.byte $A5 ; |X X  X X|		4th
	.byte $7E ; | XXXXXX |
	.byte $18 ; |   XX   |
	.byte $FF ; |XXXXXXXX|
	.byte $DB ; |XX XX XX|		8th
	.byte $18 ; |   XX   |
        .byte $00 ; |........|		16th
         .byte $24 ; |..X..X..|
         .byte $99 ; |X..XX..X|
         .byte $A5 ; |X.X..X.X|
         .byte $E7 ; |XXX..XXX|
         .byte $18 ; |...XX...|
         .byte $18 ; |...XX...|
         .byte $18 ; |...XX...|
         .byte $3C ; |..XXXX..|



QuotileSprite
Quotile_0	
	.byte $00 ; |........|
	.byte $60 ; |.XX.....|
	.byte $11 ; |...X...X|
	.byte $09 ; |....X..X|
	.byte $3A ; |..XXX.X.|
	.byte $5C ; |.X.XXX..|
	.byte $90 ; |X..X....|
	.byte $88 ; |X...X...|
	.byte $06 ; |.....XX.|
;;QUOTILE_HEIGHT = . - Quotile_0 alt way to specify height
Quotile_1	
	.byte $00 ; |........|
	.byte $04 ; |.....X..|
	.byte $62 ; |.XX...X.|
	.byte $92 ; |X..X..X.|
	.byte $1C ; |...XXX..|
	.byte $38 ; |..XXX...|
	.byte $49 ; |.X..X..X|
	.byte $46 ; |.X...XX.|
	.byte $20 ; |..X.....|
Quotile_2	
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
QuotileColor:
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
	.word Reset
	.word Reset
