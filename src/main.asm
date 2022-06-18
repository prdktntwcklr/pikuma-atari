    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; include required files for VCS memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; declare variables from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos            byte                ; player0 x-position
JetYPos            byte                ; player0 y-position
BomberXPos         byte                ; player1 x-position
BomberYPos         byte                ; player1 y-position
MissileXPos        byte                ; missile x-position
MissileYPos        byte                ; missile y-position
Score              byte                ; keeps track of score
Timer              byte                ; keeps track of time (must be right after score)
Temp               byte                ; helper variable to store temporary score values

OnesDigitOffset    word                ; lookup table offset for the score 1's digit
TensDigitOffset    word                ; lookup table offset for the score 10's digit

JetSpritePtr       word                ; pointer to player0 sprite lookup table
JetColorPtr        word                ; pointer to player0 color lookup table
BomberSpritePtr    word                ; pointer to player1 sprite lookup table
BomberColorPtr     word                ; pointer to player1 color lookup table

JetAnimOffset      byte                ; player0 sprite frame offset for animation
Random             byte                ; random number for enemy position
ScoreSprite        byte                ; store the sprite bit pattern for the score
TimerSprite        byte                ; store the sprite bit pattern for the timer

TerrainColor       byte                ; store the color of the terrain
RiverColor         byte                ; store the color of the river

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT    = 9                      ; player0 sprite height (number of rows in lookup table)
BOMBER_HEIGHT = 9                      ; player1 sprite height (number of rows in lookup table)
DIGITS_HEIGHT = 5                      ; scoreboard digit height (#rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start ROM code from memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START                        ; macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #65
    sta JetXPos                        ; JetXPos = 65
    lda #10
    sta JetYPos                        ; JetYPos = 10
    lda #54
    sta BomberXPos                     ; BomberXPos = 54
    lda #96
    sta BomberYPos                     ; BomberYPos = 96
    lda #%11010100
    sta Random                         ; Random = $D4
    lda #0
    sta Score                          ; Score = 0
    sta Timer                          ; Timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; declare a macro to check if we should display the missile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    MAC DRAW_MISSILE
        ; x has the number of scanline that we are at
        lda #$%00000000
        cpx MissileYPos                ; compare X (current scanline) with missile y-pos                
        bne .SkipMissileDraw           ; if (X != missile y-pos), do not draw missile
.DrawMissile:
        lda #%00000010                 ; enable missile0 display
        inc MissileYPos                ; MissileYPos++
.SkipMissileDraw:
        sta ENAM0                      ; store the correct value in the TIA missile register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; initialize pointers to correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr                   ; lo-byte pointer for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1                 ; hi-byte pointer for jet sprite lookup table

    lda #<JetColor
    sta JetColorPtr                    ; lo-byte pointer for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1                  ; hi-byte pointer for jet color lookup table   

    lda #<BomberSprite
    sta BomberSpritePtr                ; lo-byte pointer for bomber sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1              ; hi-byte pointer for bomber sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr                 ; lo-byte pointer for bomber color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1               ; hi-byte pointer for bomber color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VSYNC                          ; turn on VSYNC
    sta VBLANK                         ; turn on VBLANK
    REPEAT 3
        sta WSYNC                      ; display 3 recommended lines of VSYNC
    REPEND
    lda #0
    sta VSYNC                          ; turn off VSYNC
    REPEAT 33                          ; render fewer VBLANK lines because some are used below 
        sta WSYNC                      ; display the recommended lines of VBLANK
    REPEND
    
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; calculations and tasks performed in the VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos                  ; set player0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos                  ; set player1 horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos                  ; set missile horizontal position

    jsr CalculateDigitOffset           ; calculate scoreboard digit lookup table offset

    sta WSYNC
    sta HMOVE                          ; apply the horizontal offsets previously set   
    
    lda #0
    sta VBLANK                         ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0                             ; clear TIA registers before each new frame
    sta COLUBK                         ; set color background to zero

    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF                         ; disable playfield reflection

    lda #$1E
    sta COLUPF                         ; set scoreboard color to yellow

    ldx #DIGITS_HEIGHT                 ; start X counter with 5 (height of digits)
.ScoreDigitLoop:
    ldy TensDigitOffset                ; get the tens digit offset for the score
    lda Digits,Y                       ; load the bit pattern from the lookup table
    and #$F0                           ; mask the graphics for the ones digit
    sta ScoreSprite                    ; save the score tens digit pattern in a variable
    ldy OnesDigitOffset                ; get the ones digit offset for the score
    lda Digits,Y                       ; load the bit pattern from the lookup table
    and #$0F                           ; mask the graphics for the ones digit
    ora ScoreSprite                    ; merge it with the saved tens digit graphic
    sta ScoreSprite                    ; and save it
    sta WSYNC                          ; wait for the end of scanline
    sta PF1                            ; update the playfield to display the score sprite

    ldy TensDigitOffset+1              ; get the tens digit offset for the timer
    lda Digits,Y                       ; load the bit pattern from the lookup table
    and #$F0                           ; mask the graphics for the ones digit
    sta TimerSprite                    ; save the timer tens digit pattern in a variable
    ldy OnesDigitOffset+1              ; get the ones digit offset for the timer  
    lda Digits,Y                       ; load the bit pattern from the lookup table    
    and #$0F                           ; mask the graphics for the ones digit    
    ora TimerSprite                    ; merge it with the saved tens digit graphic 
    sta TimerSprite                    ; and save it

    jsr Sleep12Cycles                  ; waste 12 cycles of CPU

    sta PF1                            ; update the playfield for timer display

    ldy ScoreSprite                    ; preload for the next scanline
    sta WSYNC                          ; wait for the next scaline

    sty PF1                            ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles                  ; waste some cycles
    
    dex                                ; X--
    sta PF1                            ; update the playfield for the Timer display
    bne .ScoreDigitLoop                ; if X != 0, then branch to ScoreDigitLoop

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; display the 84 visible scanlines of our main game (2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda RiverColor
    sta COLUBK                         ; set color background to blue
    lda TerrainColor
    sta COLUPF                         ; set playfield grass color to green
    lda #$01
    sta CTRLPF                         ; enable playfield reflection

    lda #$F0
    sta PF0                            ; set PF0 bit pattern
    lda #$FC
    sta PF1                            ; set PF1 bit pattern
    lda #0
    sta PF2                            ; set PF2 bit pattern

    ldx #85                            ; X counts number of remaining scanlines

.GameLineLoop:
    DRAW_MISSILE                       ; macro to check if we should draw the missile

.AreWeInsideJetSprite:
    txa                                ; transfer X to A
    sec                                ; set carry before subtraction
    sbc JetYPos                        ; subtract sprite Y-coordinate
    cmp JET_HEIGHT                     ; are we between Y-position and sprite height
    bcc .DrawSpriteP0                  ; if result < SpriteHeight, call the draw routine
    lda #0                             ; else, set lookup index to zero

.DrawSpriteP0:
    clc                                ; clear carry flag before addition
    adc JetAnimOffset                  ; jump to the correct sprite frame address
    tay                                ; load Y so that we can work with pointer
    lda (JetSpritePtr),Y               ; load player0 bitmap data from loopup table
    sta WSYNC                          ; wait for scanline
    sta GRP0                           ; set graphics for player0
    lda (JetColorPtr),Y                ; load player0 color data from loopup table
    sta COLUP0                         ; set color for player0

.AreWeInsideBomberSprite:
    txa                                ; transfer X to A
    sec                                ; set carry before subtraction
    sbc BomberYPos                     ; subtract sprite Y-coordinate
    cmp BOMBER_HEIGHT                  ; are we between Y-position and sprite height
    bcc .DrawSpriteP1                  ; if result < SpriteHeight, call the draw routine
    lda #0                             ; else, set lookup index to zero

.DrawSpriteP1:
    tay                                ; load Y so that we can work with pointer
    lda #%00000101
    sta NUSIZ1                         ; stretch player1 sprite
    lda (BomberSpritePtr),Y            ; load player1 bitmap data from loopup table
    sta WSYNC                          ; wait for scanline
    sta GRP1                           ; set graphics for player0
    lda (BomberColorPtr),Y             ; load player0 color data from loopup table
    sta COLUP1                         ; set color for player0

    dex                                ; X--
    bne .GameLineLoop                  ; loop until X == 0

    lda #0
    sta JetAnimOffset                  ; reset jet animation frame

    sta WSYNC                          ; wait for a scanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK                         ; turn VBLANK on again
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK                         ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; process joystick inputs for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Left:
    lda #%01000000                     ; pattern for player0 joystick CheckP0Up
    bit SWCHA
    bne CheckP0Right                   ; if bit pattern does not match
.P0LeftPressed:
    lda JetXPos
    cmp #35                            ; if JetXPos < 35
    bmi CheckButtonPress               ; then, skip increment
    dec JetXPos
    lda JET_HEIGHT                     ; 9
    sta JetAnimOffset                  ; set animation offset to the second frame
    jmp CheckButtonPress               ; prevent diagonal movement

CheckP0Right:
    lda #%10000000                     ; pattern for player0 joystick CheckP0Up
    bit SWCHA
    bne CheckP0Up                      ; if bit pattern does not match
.P0RightPressed:
    lda JetXPos
    cmp #100                           ; if JetXPos > 100
    bpl CheckButtonPress               ; then, skip increment    
    inc JetXPos
    lda JET_HEIGHT                     ; 9
    sta JetAnimOffset                  ; set animation offset to the second frame
    jmp CheckButtonPress               ; prevent diagonal movement

CheckP0Up:
    lda #%00010000                     ; pattern for player0 joystick CheckP0Up
    bit SWCHA
    bne CheckP0Down                    ; if bit pattern does not match
.P0UpPressed:
    lda JetYPos
    cmp #70                            ; if JetYPos > 70
    bpl CheckButtonPress               ; then, skip increment
    inc JetYPos
    lda #0
    sta JetAnimOffset                  ; reset animation offset to the first frame

CheckP0Down:
    lda #%00100000                     ; pattern for player0 joystick CheckP0Up
    bit SWCHA
    bne CheckButtonPress               ; if bit pattern does not match
.P0DownPressed:
    lda JetYPos
    cmp #5                             ; if JetYPos < 5
    bmi CheckButtonPress               ; then, skip increment
    dec JetYPos
    lda #0
    sta JetAnimOffset                  ; reset animation offset to the first frame

CheckButtonPress:
    lda #%10000000                     ; pattern for button press
    bit INPT4
    bne EndInputCheck                  ; if bit pattern does not match
.ButtonPressed:
    lda JetXPos
    clc
    adc #5
    sta MissileXPos                    ; set missile x-position to player x-position + 5
    lda JetYPos
    clc
    adc #8
    sta MissileYPos                    ; set missile y-position to player y-position + 8

EndInputCheck:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; calculations to update bomber position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc                                ; clear carry before compare
    cmp #0                             ; compare bomber y-position with 0
    bmi .ResetBomberPosition           ; if negative, reset bomber y-position back up    
    dec BomberYPos                     ; else, decrement bomber y-position for next frame
    jmp EndBomberPositionUpdate
.ResetBomberPosition:
    jsr GetRandomBomberPosition        ; call subroutine for random x-position

.IncrementTimer:
    sed                                ; enable decimal mode for timer value
    lda Timer                          ; Timer += 1, inc does not work in BCD
    clc
    adc #1
    sta Timer
    cld                                ; disable decimal mode

EndBomberPositionUpdate:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; perform collision checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollision:
.CheckCollisionP0P1:
    lda #%10000000                     ; CXPPMM bit 7 detects P0P1 collision
    bit CXPPMM                         ; check if bit is set
    bne .P0P1Collided                  ; collision detected
    jsr SetTerrainRiverColor           ; else, set playfield color to green and blue
    jmp .CheckCollisionM0P1            ; check next possible collision
.P0P1Collided
    jsr GameOver                       ; call GameOver subroutine

.CheckCollisionM0P1:
    lda #%10000000                     ; CXM0P bit 7 detects M0P1 collision
    bit CXM0P                          ; check if bit is set
    bne .M0P1Collided                  ; collision detected
    jmp EndCollisionCheck              ; else, end collision check
.M0P1Collided:
    sed                                ; increment score in BCD mode
    lda Score
    clc
    adc #1
    sta Score
    cld
    lda #0                             ; reset missile position to make it invisible
    sta MissileYPos

EndCollisionCheck:                     ; fallback
    sta CXCLR                          ; clear all collision flags before next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; loop back to start a new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame                     ; continue to display next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; subroutine to set the colors for the terrain and river to green and blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor                   ; set color to green
    lda #$84
    sta RiverColor                     ; set color to blue
    lda #$
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; subroutine to handle object horizontal position with fine offset
; 
; A is the target x-coordinate in pixels
; Y is the type of object (0: player0, 1: player1, 2: missile0,
;                          3: missile1, 4: ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
SetObjectXPos subroutine
    sta WSYNC                          ; start fresh new scanline
    sec                                ; make sure carry-flag is set before subtracion
.Div15Loop
    sbc #15                            ; subtract 15 from accumulator
    bcs .Div15Loop                     ; loop until carry-flag is clear
    eor #7                             ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                                ; four shift lefts to get only the top 4 bits
    sta HMP0,Y                         ; store the fine offset to the correct HMxx
    sta RESP0,Y                        ; fix object position in 15-step increment
    rts                                ; return from subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; subroutine to end the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta TerrainColor                   ; set TerrainColor to red
    sta RiverColor                     ; set RiverColor to red
    lda #0
    sta Score                          ; Score = 0
    rts                                ; return from subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; subroutine to generate a Linear-Feedback Shift Register random number
;
; generate a LSFR random number
; divide the random value by four to limit the range of the result to match river
; add 30 to comepnsate for the left green playfield 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPosition subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random                         ; performs a series of shifts and bit operations

    lsr
    lsr                                ; two right shifts to divide by four
    adc #30                            ; add offset to compensate for left playfield          
    sta BomberXPos

    lda #96                  
    sta BomberYPos                     ; place bomber at the top of the screen
    rts                                ; return from subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; convert high and low nibbles of the variables Score and Timer
; into the offsets of digits lookup table so the values can be displayed
; each digit has a height of 5 bytes in the lookup table
; 
; for the lower nibble, we need to multiply by 5
; - we can use left shifts to perform multiplication by 2
; - for any number N, the value of N*5 = (N*2*2)+N
:
; for the upper nibble, since it's already times 16, we need to divide by it
; by 16 and then multiply by 5
; - we can use right shifts to perform division by 2
; - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                             ; X is the loop counter
.PrepareScoreLoop                      ; this will loop twice, first X=1, then X=0
    lda Score,X                        ; load A with Timer (X = 1) or Score (X = 0)
    and #$0F                           ; remove the tens digit by masking top 4 bits
    sta Temp                           ; save the value of A into Temp
    asl                                ; shift left (it is now N*2)
    asl                                ; shift left (it is now N*4)
    adc Temp                           ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X              ; store Timer (X = 1) and Score (X = 0) offset

    lda Score,X                        ; load A with Timer (X = 1) or Score (X = 0)
    and #$F0                           ; remove the ones digit by masking bottom 4 bits
    lsr                                ; shift right (it is now N/2)
    lsr                                ; shift right (it is now N/4)
    sta Temp                           ; save the value of A into Temp
    lsr                                ; shift right (it is now N/8)
    lsr                                ; shift right (it is now N/16)
    adc Temp                           ; add the value saved in Temp (+N/4)
    sta TensDigitOffset,X              ; store Timer (X = 1) and Score (X = 0) offset             

    dex                                ; X--
    bpl .PrepareScoreLoop              ; while X >= 0 loop to pass a second time
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; jsr takes 6 cycles
; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; declare ROM lookup tables for graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
Digits:
    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01010101                    ; # # # #
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###

    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #

    .byte %01110111                    ; ### ###
    .byte %00010001                    ;   #   #
    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01110111                    ; ### ###

    .byte %01110111                    ; ### ###
    .byte %00010001                    ;   #   #
    .byte %00110011                    ;  ##  ##
    .byte %00010001                    ;   #   #
    .byte %01110111                    ; ### ###

    .byte %01010101                    ; # # # #
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #

    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01110111                    ; ### ###
    .byte %00010001                    ;   #   #
    .byte %01110111                    ; ### ###

    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###

    .byte %01110111                    ; ### ###
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #
    .byte %00010001                    ;   #   #

    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###

    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###
    .byte %00010001                    ;   #   #
    .byte %01110111                    ; ### ###

    .byte %00100010                    ;  #   #
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01010101                    ; # # # #

    .byte %01110111                    ; ### ###
    .byte %01010101                    ; # # # #
    .byte %01100110                    ; ##  ##
    .byte %01010101                    ; # # # #
    .byte %01110111                    ; ### ###

    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01000100                    ; #   #
    .byte %01000100                    ; #   #
    .byte %01110111                    ; ### ###

    .byte %01100110                    ; ##  ##
    .byte %01010101                    ; # # # #
    .byte %01010101                    ; # # # #
    .byte %01010101                    ; # # # #
    .byte %01100110                    ; ##  ##

    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01110111                    ; ### ###

    .byte %01110111                    ; ### ###
    .byte %01000100                    ; #   #
    .byte %01100110                    ; ##  ##
    .byte %01000100                    ; #   #
    .byte %01000100                    ; #   #

JetSprite:
    .byte #%00000000                   ;
    .byte #%00010100                   ;   # #
    .byte #%01111111                   ; #######
    .byte #%00111110                   ;  #####
    .byte #%00011100                   ;   ###
    .byte #%00011100                   ;   ###
    .byte #%00001000                   ;    #
    .byte #%00001000                   ;    #
    .byte #%00001000                   ;    #

JetSpriteTurn:
    .byte #%00000000                   ;
    .byte #%00001000                   ;    #
    .byte #%00111110                   ;  #####
    .byte #%00011100                   ;   ###
    .byte #%00011100                   ;   ###
    .byte #%00011100                   ;   ###
    .byte #%00001000                   ;    #
    .byte #%00001000                   ;    #
    .byte #%00001000                   ;    #

BomberSprite:
    .byte #%00000000                   ;
    .byte #%00001000                   ;    #
    .byte #%00001000                   ;    #
    .byte #%00101010                   ;  # # #
    .byte #%00111110                   ;  #####
    .byte #%01111111                   ; #######
    .byte #%00101010                   ;  # # #
    .byte #%00001000                   ;    #
    .byte #%00011100                   ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fill ROM to 4kB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset                          ; program reset address
    word Reset                          ; interrupt vector (unused)
