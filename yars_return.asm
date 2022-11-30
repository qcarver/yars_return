;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2 line kernal score examples and more pinched from https://pikuma.com/courses
;;	qcarver@gmail.com November of 2022 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
YarYPos		byte	;
QuotileXPos	byte	;
QuotileYPos	byte	;
MissileXPos	byte	;
MissileYPos	byte	;
MissileData	byte	;
YarSpritePtr	word	; Pointers
QtylSpritePtr	word	;
BgColor		byte	;
Scratch		byte	;
YarAnimOffset	byte	;
Random		byte	;
Score		byte	; w/ Bomber code, impt that			2digitvalue 
Timer		byte	;		Score and timer be side by side	2digitvalue
ScoreSprite	byte	; store the score bit patter
TimerSprite	byte	; store the timer bit pattern
OnesDigitOffset word	; LUT offset for 1's digit
TensDigitOffset word	; LUT offset for 10's digit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_YAR_HEIGHT	equ 9		; Height of Yar Sprite
_QUOTILE_HEIGHT	equ 9		; Height of Quotile
_DIGITS_HEIGHT	equ 5		; Scoreboard digit
_YAR_N		equ 9		; Offsets
_YAR_S		equ 0		;	for Yar
_YAR_W		equ 18		;	Sprites
_YAR_NW		equ 27		;		by direction
_YAR_SW		equ 36		;		
_UP		equ #%00010000	; bitmasks
_DOWN		equ #%00100000	;	for missile
_LEFT		equ #%01000000	;		and P0 joystick
_RIGHT		equ #%10000000	;		direction
_DIR_MASK	equ #%11110000	; mask to copy all direction bits above
_INPT4_FIRED	equ #%10000000  ; bitmask to check P0 joystick button press
_FLYOUT		equ #%00000001  ; bitmask for Flyout (is missile in flight)


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
	lda #0
	sta Score
	lda #1
	sta Timer
	lda #80
	sta YarXPos
	lda #52
	sta QuotileXPos 
	lda #8
	sta QuotileYPos
	lda #69
	sta Random
	lda #$A0
	sta BgColor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to LUTs 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #<_YAR_Sprite
	sta YarSpritePtr	;lo-byte for address of Yar sprite
	lda #>_YAR_Sprite
	sta YarSpritePtr+1	;hi-byte for address of Yar sprite	

	lda #<QuotileSprite
	sta QtylSpritePtr	;lo-byte for address of Quotile sprite
	lda #>QuotileSprite
	sta QtylSpritePtr+1	;hi-byte for address of Quotile sprite	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK		; turn on VBlank
	sta VSYNC		; turn on VSYNC
	REPEAT 3
		sta WSYNC	; display 3 recommended lines of VSYNC
	REPEND
	lda #0
	sta VSYNC		; turn off VSYNC
	REPEAT 33		; instead of 37 
		sta WSYNC	; display VBLANK lines
	REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBlank 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda YarXPos		;3	; this will be A parameter for subr
	ldy #0			;3,6	; this will be object type paramter 
	jsr SetObjXPos		;78,84	; set Yar horiz position, call subroutine

	lda QuotileXPos		;3,87	; same as above, but Qtile instead of Yar
	ldy #1			;2,89
	jsr SetObjXPos		;78,167	; set player1 horizontal position

	lda MissileXPos		;3,170	; same as above, but for Missile 
	ldy #2			;2,172	; 
	jsr SetObjXPos		;78,250	; set Missile horizontal position 
	
	jsr CalcDigitOffset	;111,361; calc scoreboard digits offset

	sta WSYNC ;-----------------------------------------------------------*
	sta HMOVE		;3	; apply offsets
	sta VBLANK		;3,6	; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0			;2,8
	sta PF0			;3,11	; reset
	sta PF1			;3,14	;	TIA registers
	sta PF2			;3,17	;		before
	sta GRP0		;3,20	;		displaying
	sta GRP1		;3,23	;			score 
	lda #$1E		;2,25
	sta COLUPF		;3,28	; set score number color to yellow
	ldx #_DIGITS_HEIGHT	;2,30	; start X counter with 5 (height of digits)
	
.ScoreDigitLoop:
	ldy TensDigitOffset	;?,??	; get the tens digit offset for the Score
	lda Digits,Y		;?,??	; load the bit pattern from lookup table
	and #$F0		;?,??	; mask/remove the graphics for the ones digit
	sta ScoreSprite		;?,??	; save the score tens digit pattern in a variable
	
	ldy OnesDigitOffset	;?,??	; get the ones digit offset for the Score
	lda Digits,Y		;?,??	; load the digit bit pattern from lookup table
	and #$0F		;?,??	; mask/remove the graphics for the tens digit
	ora ScoreSprite		;?,??	; merge it with the saved tens digit sprite
	sta ScoreSprite		;?,??	; and save it
	sta WSYNC		;?,??	; wait for the end of scanline
	sta PF1			;?,??	; update the playfield to display the Score sprite
	
	ldy TensDigitOffset+1	;?,??	; get the left digit offset for the Timer
	lda Digits,Y		;?,??	; load the digit pattern from lookup table
	and #$F0		;?,??	; mask/remove the graphics for the ones digit
	sta TimerSprite		;?,??	; save the timer tens digit pattern in a variable
	
	ldy OnesDigitOffset+1	;?,??	; get the ones digit offset for the Timer
	lda Digits,Y		;?,??	; load digit pattern from the lookup table
	and #$0F		;?,??	; mask/remove the graphics for the tens digit
	ora TimerSprite		;?,??	; merge with the saved tens digit graphics
	sta TimerSprite		;?,??	; and save it ...commented back in
	
	jsr Sleep12Cycles	;?,??	; wastes some cycles
	
	sta PF1			;?,??	; update the playfield for Timer display
	
	ldy ScoreSprite		;?,??	; preload for the next scanline
	sta WSYNC		;?,??	; wait for next scanline
	
	sty PF1			;?,??	; update playfield for the score display
	inc TensDigitOffset
	inc TensDigitOffset+1
	inc OnesDigitOffset
	inc OnesDigitOffset+1	;?,??	; increment all digits for the next line of data
	
	jsr Sleep12Cycles	;?,??	; waste some cycles
	
	dex			;?,??	; X--
	sta PF1			;?,??	; update the playfield for the Timer display
	bne .ScoreDigitLoop	;?,??	; if dex != 0, then branch to ScoreDigitLoop
	
	sta WSYNC
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visible Frame: display 192 scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine
	ldy BgColor		;?,??	; load bg color
	sty COLUBK		;?,??	; 		into bg color register
 
	ldx #96			;?,??	; row cnt:192:1,96:2line kernel 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visible Row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.GameLineLoop:
		;;BEGIN: Draw Playfield______________
	lda PFColors,x		;?,??	; get color...
	sta COLUPF		;?,??	; for row
	lda PF0DataA,x		;?,??	; get bit..
	sta PF0			;?,??	; patterns
	lda PF1DataA,x		;?,??	; for the
	sta PF1			;?,??	; three 
	lda PF2DataA,x		;?,??	; playfield
	sta PF2			;?,??	; vars
	sta WSYNC
	;;END: Draw Playfield________________

.DrawMissile:
	lda #%00000000		;?,??	; Value to make missile invisible
	cpx MissileYPos		;?,??	; Test: is missile on this scanline? 
	bne .TestYarOnLine	;?,??	; if not: skip to next section 
	lda #%00000010		;?,??	; 	if so: enable the missle 
.TestYarOnLine:
	sta ENAM0		;?,??	; visible or invisible...draw missile 
	txa			;?,??	; Subtract the 
	sec			;?,??	; 	line position from	
	sbc YarYPos		;?,??	; 	the yar position	
	cmp #_YAR_HEIGHT		;?,??	; if the result 
	bcc .Draw_YAR_Sprite	;?,??	; is positive: yar is on scanline
	lda #0			;?,??	; else: set  empty sprite line

.Draw_YAR_Sprite		
	clc			;add
	adc YarAnimOffset	;?,??	; Yar animation offset
		tay
	lda (YarSpritePtr),y	;ld Sprite at offset
	;sta WSYNC		;?,??	; wait for scanline
	sta GRP0		;?,??	; set graphics for Yar
	lda #$0E		;load color
	sta COLUP0		;?,??	; set color of Yar

.AreWeInsideQuotileSprite:
	txa			;?,??	; transfer linePos to A
	sec			;?,??	; sec (before subtract)
	sbc QuotileYPos		;?,??	; SpriteY - linepos
	cmp #_QUOTILE_HEIGHT		;?,??	; w/in height of sprite?
	bcc .DrawQuotileSprite
	lda #0			;?,??	; else: set  empty sprite line

.DrawQuotileSprite
	tay			;?,??	; hint: Y only indirect register
	lda #5			;?,??	; b101 stretches sprite Horizon
	sta NUSIZ1
	lda (QtylSpritePtr),y	;ld Sprite at offset
	sta WSYNC		;?,??	; wait for scanline
	sta GRP1		;?,??	; set graphics for Quotile
	lda #$46		;?,??	; red
	sta COLUP1		;?,??	; set color of Quotile

	dex			;?,??	; x--
	bne .GameLineLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK		;?,??	; turn on VBLANK again
	REPEAT 30
	sta WSYNC		;?,??	; 30 blank lines of overscan 
	REPEND
	lda #0
	sta VBLANK		;?,??	; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckP0_UP:
	lda #_UP		;?,??	; player0 joystick up
	bit SWCHA	
	bne CheckP0_DOWN	;?,??	; If bit pattern doesn't bypass UP
	inc YarYPos	
	lda #_YAR_N		;?,??	; set Yar glpyh
	sta YarAnimOffset	;?,??	; 	to North Facing
CheckP0_DOWN:
	lda #_DOWN 		;?,??	; player0 joystick down
	bit SWCHA
	bne CheckP0_LEFT 	;?,??	; If bit pattern doesn't match, bypass #_DOWN Block
	lda #_YAR_S		;?,??	; set Yar glpyh
	sta YarAnimOffset	;?,??	; 	to South Facing
	lda #6			;?,??	; Yar can't 
	clc			;?,??	; fly below
	cmp YarYPos		;?,??	; 	the deck
	bpl CheckP0_LEFT 	;?,??	; skip decrement if he tries to 
	dec YarYPos	
CheckP0_LEFT:
	lda #_LEFT		;?,??	; player0 joystick left
	bit SWCHA
	bne CheckP0_RIGHT 	;?,??	; If bit pattern doesn't match, bypass #_LEFT Block
	dec YarXPos	
	lda #_YAR_W		;?,??	; set Yar glpyh
	sta YarAnimOffset	;?,??	; 	to North Facing
	lda 0			;?,??	; reflect		
	STA REFP0		;?,??	; player 0
CheckP0_RIGHT:
	lda #_RIGHT 		;?,??	; player0 joystick right
	bit SWCHA
	bne CheckFire		;?,??	; If bit pattern doesn't match, bypass #_RIGHT Block
	inc YarXPos
	lda #_YAR_W		;?,??	; set Yar glpyh
	sta YarAnimOffset	;?,??	; 	to North Facing
	lda 8			;?,??	; reflect		
	STA REFP0		;?,??	; player 0
CheckFire:
	lda #_INPT4_FIRED	;?,??	; check if the Fire
	bit INPT4		;?,??	; 	button is pressed
	bne EndInputCheck	;?,??	; Button not pressed skip to end
	lda #_DIR_MASK		;?,??	; Copy P0 joystick
	and SWCHA		;?,??	; direction data and
	ora #_FLYOUT		;?,??	; 	flyout (fired!) flag
	sta MissileData		;?,??	; 		into MissileData
	lda YarYPos		;?,??	; Missile starts
	clc			;?,??	; 	4 pixels 
	adc #4			;?,??	; 	inside
	sta MissileYPos		;?,??	; 			Yar Y Position	
	lda YarXPos		;?,??	; Missile starts
	clc			;?,??	; 	4 pixels 
	adc #4			;?,??	; 	inside
	sta MissileXPos		;?,??	; 	Yar X Position	
EndInputCheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Missile Flyout 
;; Recall MissileData MSNybble (like joystick data) is inverted logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #_FLYOUT		;?,??	; if the missile
	bit MissileData		;?,??	; is not in flyout	
	beq ResetMissile	;?,??	; 	skip to resetting missile
FlyoutUp
	lda #_UP		;?,??	; if the missile
	bit MissileData		;?,??	; 	is not in UP	
	bne FlyoutDown		;?,??	; then: skip to check for Down	
	inc MissileYPos		;?,??	; else:	subtract from Y Pos
	inc MissileYPos		;?,??	; else:	subtract from Y Pos
	jmp FlyoutRight		;?,??	; #_UP & #_DOWN are mutually exclusive, skip it
FlyoutDown
	lda #_DOWN		;?,??	; if the missile flyout
	bit MissileData		;?,??	; 	is not Down 
	bne FlyoutRight		;?,??	; then: skop to check for Right
	dec MissileYPos		;?,??	; else: add to Y Pos
	dec MissileYPos		;?,??	; else: add to Y Pos
FlyoutRight
	lda #_RIGHT		;?,??	; if the missile flyout
	bit MissileData		;?,??	; 	is not RIGHT 
	bne FlyoutLeft		;?,??	; then: skip to check for Left
	inc MissileXPos		;?,??	; else: add to X Pos
	inc MissileXPos		;?,??	; else: add to X Pos
	jmp QuotileCalc		;?,??	; #_RIGHT & #_LEFT are mutually xor, skip it
FlyoutLeft
	lda #_LEFT		;?,??	; if the missile flyout
	bit MissileData		;?,??	; 	is not LEFT 
	bne QuotileCalc		;?,??	; then: skip this block
	dec MissileXPos		;?,??	; 	else: subtract from X Pos
	dec MissileXPos		;?,??	; 	else: subtract from X Pos
	jmp QuotileCalc
;;TODO..drop bombs if joystick direction was 11110000 on Firing
	
ResetMissile
	and MissileData, #$FE	;?,??	; clear the #_FLYOUT flag, leave the rest
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update Quotile position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
QuotileCalc
	lda QuotileYPos
	clc
	cmp #96
	bmi IncrementQuotile	;?,??	; if QuotleYPos < 96
	sed			;?,??	; Enter BCD mode (scoreboard is base-10)
	lda Score		;
	clc			;?,??	; Accumulator doeesn't have
	adc #1			;?,??	; increment a (A++)	
	sta Score
	lda Timer
	clc
	adc #1
	sta Timer
	cld			;?,??	; exit BCD mode
	lda 0			;?,??	; else load 0	
	sta QuotileYPos		;?,??	; 	into QuotileYPos
	jsr GetRndByte		;?,??	; Get Rand byte and put in A 
	lsr			;?,??	; Divide rnd byte by two (hint Hrez)
	sta QuotileXPos		;?,??	; 	Store rnd in QuotileXPos
IncrementQuotile
	inc QuotileYPos		;?,??	; Typical

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check collision between player0 and the playfield 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldy #$A0		;?,??	; Assume sky will be blue (no collision)
	lda #%10000000		;?,??	; hi bit of CXPPMM 
	bit CXPPMM		;?,??	; informs of a Player0, Player1 collision
	beq BlueSky		;?,??	; If There is no colision jump to Blue sky
	ldy #$30		;?,??	; else red 
BlueSky
	sty BgColor		;?,??	; Blue or Red gets stored in our Background color field
	sta CXCLR		;?,??	; Reset all collision flags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jmp StartFrame		;?,??	; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position w/ fine offset 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our target
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missle1, 4:ball) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjXPos subroutine
	sta WSYNC		;3,	; start a fresh new scanline
	sec			;2,5	; set the carry-flag before subtracting
.Div15Loop			;	; divide is just continuos subtraction
	sbc #15			;	; subtract 15 from A in accumulator
	bcs .Div15Loop		;,<52	; loop until carry-flag is clear
	eor #7			;2,54	; xor signs corse remainder from -8 to 7
	asl			;2,56
	asl			;2,58
	asl			;2,60
	asl			;2,62	; move four lowest bits to hi (littleendian)
	sta HMP0,Y		;5,67	; store the fine offset to the correct HMxx
	sta RESP0,Y		;5,72	; fix object position in 15 step increment
	rts			;6,78	; return	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to put a random number in Quotile X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a random number using a Linear-Feedback Shift Register  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRndByte subroutine
	lda Random		;?,??	; Load starting random seed
	asl			;?,??	; <<1
	eor Random		;?,??	; xor
	asl			;?,??	; <<1
	eor Random		;?,??	; xor
	asl
	asl
	eor Random
	asl
	rol Random		;?,??	; performs a series of shifts and bit operations
	rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get offsest addr of  5 pixels tall digits. so step will be (digit * 5) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalcDigitOffset subroutine
	ldx #1			;2	; X register is the loop counter
.PrepareScoreLoop			; loops twice: score, then timer
	lda Score,X		;4,6	; load score+x(digits+1//timer|+0//score)
	and #%00001111 		;2,8	; mask 10's part of digits, gets 1's part
	sta Scratch		;3,11	; save the value of A into Temp
	asl			;2,13	; Multiply
	asl			;2,15	; 		by
	adc Scratch		;2,17	; 		five 	(hint: n*2*2+n) 
	sta OnesDigitOffset,x	;4,19	; save A in OnesDigitOffset + 1
		
	lda Score,x		;4,23	; load A w/ x = 1 or timer
	and #$F0		;2,25	; mask out 1's digit, get the 10's side
	lsr			;2,27	; divide value of hi digit by 16
	lsr			;2,29	; then multiply	
	sta Scratch		;3,32	; 	by 5 to get	
	lsr			;2,34	; 		it's offset	
	lsr			;2,36	; 			into LUT	
	adc Scratch		;2,38	; hint: (add n/4 to n/16 same as (n/16)*5
	sta TensDigitOffset,x	;4,44	; store A in TensDigitOffset or ""+1

	dex			;2,46	; x--
	bpl .PrepareScoreLoop	;3,49	; do while x is positive
	rts			;105	; (stuff + loops twice + stuff)

;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
	rts

Digits:
	.byte %00100010	;  #   # 
	.byte %01010101	; # # # #
	.byte %01010101	; # # # #
	.byte %01010101	; # # # #
	.byte %00100010	;  #   # 

	.byte %00010001	;   #	#
	.byte %00110011	;  ##  ##
	.byte %00010001	;   #	#
	.byte %00010001	;   #	#
	.byte %00010001	;   #	#

	.byte %01110111	; ### ###
	.byte %00010001	;   #	#
	.byte %00110011	;  ##  ##
	.byte %01000100	;   #	#
	.byte %01110111	; ### ###

	.byte %01110111	; ### ###
	.byte %00010001	;   #	#
	.byte %00110011	;  ##  ##
	.byte %00010001	;   #	#
	.byte %01110111	; ### ###

	.byte %01010101	; # # # #
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###
	.byte %00010001	;   #	#
	.byte %00010001	;   #	#

	.byte %01110111	; ### ###
	.byte %01000100	; #   #
	.byte %01110111	; ### ###
	.byte %00010001	;   #	#
	.byte %01100110	; ##  ## 

	.byte %00110011	;  ##  ##
	.byte %01000100	; #   #
	.byte %01110111	; ### ###
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###

	.byte %01110111	; ### ###
	.byte %00010001	;   #	#
	.byte %00010001	;   #	#
	.byte %00100010	;  #	# 
	.byte %00100010	;  #	# 

	.byte %01110111	; ### ###
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###

	.byte %01110111	; ### ###
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###
	.byte %00010001	;   #	#
	.byte %01100110	; ##  ## 

	.byte %00100010	;  #   #
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###
	.byte %01010101	; # # # #
	.byte %01010101	; # # # #

	.byte %01110111	; ### ###
	.byte %01010101	; # # # #
	.byte %01100110	; ##  ##
	.byte %01010101	; # # # #
	.byte %01110111	; ### ###

	.byte %00100010	;  #   # 
	.byte %01010101	; # # # #
	.byte %01000100	; #   #
	.byte %01010101	; # # # #
	.byte %00100010	;  #   # 

	.byte %01100110	; ##  ##
	.byte %01010101	; # # # #
	.byte %01010101	; # # # #
	.byte %01010101	; # # # #
	.byte %01100110	; ##  ##

	.byte %01110111	; ### ###
	.byte %01000100	; #   #
	.byte %01110111	; ### ###
	.byte %01000100	; #   #
	.byte %01110111	; ### ###

	.byte %01110111	; ### ###
	.byte %01000100	; #   #
	.byte %01100110	; ##  ##
	.byte %01000100	; #   #
	.byte %01000100	; #   #


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Graphic and Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_YAR_Sprite:
;Offset Legend
;JoystickPos 00010000 Heads N	GlyphOffset	16
;JoystickPos 00100000 Heads S	GlyphOffset	32
;JoystickPos 01000000 Heads W	GlyphOffset	64
;JoystickPos 01010000 Heads NW	GlyphOffset	80
;JoystickPos 01100000 Heads SW	GlyphOffset	96
;JoystickPos 10000000 Heads E	GlyphOffset	64	and reflect
;JoystickPos 10010000 Heads NE	GlyphOffset	80	and reflect
;JoystickPos 10100000 Heads SE	GlyphOffset	96	and reflect

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Yar bitpatterns courtesy of Dennis Debro, dissasembly of Scott H. Warsaw code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_YAR_N
	.byte $00 ; |........|
	.byte $24 ; |..X..X..|
	.byte $18 ; |...XX...|
	.byte $24 ; |..X..X..|
	.byte $24 ; |..X..X..|
	.byte $7E ; |.XXXXXX.|
	.byte $5A ; |.X.XX.X.|
	.byte $DB ; |XX.XX.XX|
	.byte $3C ; |..XXXX..|
;_YAR_S				;+9
	.byte $00 ; |........|	;
	.byte $3C ; |..XXXX..|
	.byte $DB ; |XX.XX.XX|
	.byte $5A ; |.X.XX.X.|
	.byte $7E ; |.XXXXXX.|
	.byte $24 ; |..X..X..|
	.byte $24 ; |..X..X..|
	.byte $18 ; |...XX...|
	.byte $24 ; |..X..X..|
;_YAR_W				;+18
	.byte $00 ; |........|
	.byte $02 ; |......X.|
	.byte $0E ; |....XXX.|
	.byte $99 ; |X..XX..X|
	.byte $67 ; |.XX..XXX|
	.byte $67 ; |.XX..XXX|
	.byte $99 ; |X..XX..X|
	.byte $0E ; |....XXX.|
	.byte $02 ; |......X.|
;_YAR_NW					;+27
	.byte $00 ; |........|
	.byte $20 ; |..X.....|
	.byte $30 ; |..XX....|
	.byte $ED ; |XXX.XX.X|
	.byte $47 ; |.X...XXX|
	.byte $2C ; |..X.XX..|
	.byte $3F ; |..XXXXXX|
	.byte $17 ; |...X.XXX|
	.byte $36 ; |..XX.XX.|
;_YAR_SW 			;+36
	.byte $00 ; |........|
	.byte $36 ; |..XX.XX.|
	.byte $17 ; |...X.XXX|
	.byte $3F ; |..XXXXXX|
	.byte $2C ; |..X.XX..|
	.byte $47 ; |.X...XXX|
	.byte $ED ; |XXX.XX.X|
	.byte $30 ; |..XX....|
	.byte $20 ; |..X.....|
;original graphics
;YarHover
	.byte $00 ; |........|
	.byte $00 ; |	     |
	.byte $24 ; |  X  X  |
	.byte $99 ; |X  XX  X|
	.byte $BD ; |X XXXX X|	
	.byte $A5 ; |X X  X X|
	.byte $7E ; | XXXXXX |
	.byte $18 ; |	XX   |
	.byte $FF ; |XXXXXXXX|
	.byte $DB ; |XX XX XX|
	.byte $18 ; |	XX   |
;YarHover_
	.byte $00 ; |........|
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
;;_QUOTILE_HEIGHT = . - Quotile_0 alt way to specify height
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Playfield Graphics and Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; courtesy of bg building tool at: https://alienbill.com/2600/atari-background-builder/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
