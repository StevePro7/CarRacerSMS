

loop:
    jp loop



; --------------------------------------------------------------
; SUBROUTINES
; --------------------------------------------------------------
; PREPARE VRAM.
; Set up vdp to recieve data at vram address in HL.

vrampr:
       push af
       ld a,l
       out ($bf),a
       ld a,h
       or $40
       out ($bf),a
       pop af
       ret

; --------------------------------------------------------------
; WRITE TO VRAM
; Write BC amount of bytes from data source pointed to by HL.
; Tip: Use vrampr before calling.

vramwr ld a,(hl)
       out ($be),a
       inc hl
       dec bc
       ld a,c
       or b
       jp nz,vramwr
       ret

; --------------------------------------------------------------
; LOAD SPRITE ATTRIBUTE TABLE
; Load data into sprite attribute table (SAT) from the buffer.

ldsat  ld hl,$3f00         ; point to start of SAT in vram.
       call vrampr         ; prepare vram to recieve data.
       ld b,255            ; amount of bytes to output.
       ld c,$be            ; destination is vdp data port.
       ld hl,satbuf        ; source is start of sat buffer.
       otir                ; output buffer to vdp.
       ret

; --------------------------------------------------------------
; UPDATE SAT BUFFER
; Generate vpos, hpos and cc data for the sprites that make up
; each of the cars (player, Mae and Ash).
; Also generate char code (cc) data from hiscore and score.

; Generate sat buffer data from player's x,y coordinates.

upbuf  ld a,(ply)          ; load player's current y-coordinate.
       ld hl,plrvp         ; point to sat buffer.
       call cary           ; refresh buffer according to y.

       ld a,(plx)          ; load player's current x-coordinate.
       ld hl,plrhp         ; point to sat buffer.
       call carx           ; refresh buffer according to x.

; Generate sat buffer data from Mae's x,y coordinates.

       ld a,(maey)         ; load Mae's current y-coordinate.
       ld hl,maevp         ; point to sat buffer.
       call cary           ; refresh buffer according to y.

       ld a,(maex)         ; load Mae's current x-coordinate.
       ld hl,maehp         ; point to sat buffer.
       call carx           ; refresh buffer according to x.

; Generate sat buffer data from Ash's x,y coordinates.

       ld a,(ashy)         ; load Ash's current y-coordinate.
       ld hl,ashvp         ; point to sat buffer.
       call cary           ; refresh buffer according to y.

       ld a,(ashx)         ; load Ash's current x-coordinate.
       ld hl,ashhp         ; point to sat buffer.
       call carx           ; refresh buffer according to x.

; Convert the hiscore in ram to char codes in buffer.

       ld hl,hscore        ; point to hiscore
       ld de,highcc        ; point to first digit's cc.
       ld b,4              ; four digits.
-      ld a,(hl)           ; load value of hscore digit.
       add a,48            ; convert it to relevant tile index.
       ld (de),a           ; put tile index in buffer.
       inc hl              ; point to next hiscore digit
       inc de              ; skip over hpos byte in buffer.
       inc de              ; point to next char code (cc) byte.
       djnz -              ; do it for all four digits.

; Convert the score in ram to char codes in buffer.

       ld hl,score         ; just like we did with hiscore...
       ld de,scorcc
       ld b,4
-      ld a,(hl)
       add a,64
       ld (de),a
       inc hl
       inc de
       inc de
       djnz -
       ret

; --------------------------------------------------------------
; CAR Y TO SPRITES' VERTICAL POSITIONS (VPOS) IN BUFFER.
; Generate vpos sat buffer data from a car's y position.
; A = car's y (i.e. ply), HL = buffer address of car vpos.

cary   ld b,4              ; a car is 4 tiles wide.
-      push af             ; a row of 4 tiles share the same y,
       push af             ; so here the y's are saved on stack.
       push af
       push af
       add a,8             ; next row is 8 pixels below.
       djnz -              ; make 4 consecutive rows.

       ld de,15            ; load buffer offset into DE.
       add hl,de           ; add buffer offset to HL.
       ld b,16             ; we need to update 16 bytes.
-      pop af              ; get saved y from stack.
       ld (hl),a           ; write it to the buffer.
       dec hl              ; point to previous byte.
       djnz -              ; backwards from vpos+15 to vpos+0.
       ret

; --------------------------------------------------------------
; CAR X TO SPRITES' HORIZONTAL POSITIONS (HPOS) IN BUFFER.
; Generates hpos sat buffer data from a car's x position.
; A = car's x (i.e. plx), HL = buffer address of car hpos.

carx   ld c,a              ; save hpos in C
       .rept 4             ; wladx: Repeat code four times.
       ld a,c              ; load hpos into A
       ld b,4              ; loop: Repeat four times.
-      ld (hl),a           ; write value to buffer at address.
       inc hl              ; skip over the char code byte.
       inc hl              ; point to next hpos byte in buffer.
       add a,8             ; add 8 (a tile's width in pixels).
       djnz -              ; jump back
       .endr               ; end of wladx repeat directive.
       ret

; --------------------------------------------------------------
; SET CAR SPRITES' CHARACTER CODES (CC)
; HL = pointer to 16 byte char codes block, DE = buffer index.

carcc ld bc,16
-      ldi
       inc de
       ld a,b
       or c
       jp nz,-
       ret

; --------------------------------------------------------------
; ADD TO SCORE.
; Add points to the score.
; HL = address of the digit we want to increase.
; B = the amount by which to increase the digit.

addsco ld a,(hl)           ; get the value of the digit.
       add a,b             ; add the amount to this value.
       ld (hl),a           ; put updated digit back in string.
       cp 9                ; test updated digit.
       ret c               ; if 9 or less, relax and return.
       ret z

; Update the next digit to the left.

       sub 10              ; make digit '0'.
       ld (hl),a           ; and load it into position.
-      dec hl              ; move pointer to next digit (left).
       inc (hl)            ; increase that digit.
       ld a,(hl)           ; load value into A.
       cp 9                ; test it like before.
       ret c               ; if 9 or less, then scoring is done.
       ret z               ;
       sub 10              ; else -  make digit '0'.
       ld (hl),a           ; and load it into position.
       jp -                ; update and test next digit.

; --------------------------------------------------------------
; UPDATE ENEMY.
; Calculate new x,y positions for an enemy car.
; IX = start of enemy data block.

enemy  ld a,(ix+0)         ; test direction.
       cp 0                ; moving left (0=left, 1=right)?
       jp nz,enem0         ; no - then enemy is moving right.

; Direction: Left - test left border.

       ld a,(ix+2)         ; load enemy's x-coordinate.
       cp leftb            ; compare it to left border constant.
       jp nc,+             ; branch if accumulator (x) > leftb.
                           ; else - enemy is on the left border
       ld a,1              ; shift direction to 'right'.
       ld (ix+0),a         ; load it into direction byte.
       jp enem1            ; skip forward to vertical movement.

; Direction: Left - subtract from enemy x coordinate.

+      ld b,hspeed         ; load horizontal speed into B.
       ld a,(ix+2)         ; load enemy x into A.
       sub b               ; subtract hspeed from x (move left).
       ld (ix+2),a         ; update enemy x coordinate.
       jp enem1            ; skip forward to vertical movement.

; Direction: Right - test right border.

enem0  ld a,(ix+2)         ; load enemy x.
       cp rightb           ; compare it to right border.
       jp c,+              ; skip if rightb > accumulator (x).

       xor a               ; else - shift direction to 0 = left.
       ld (ix+0),a         ; load new value into direction var.
       jp enem1            ; forward to vertical movement.

; Direction: Right - add to enemy x coordinate.

+      ld b,hspeed         ; load hspeed constant into B.
       ld a,(ix+2)         ; load enemy x into A.
       add a,b             ; add hspeed to enemy x.
       ld (ix+2),a         ; update enemy x coordinate.

; Vertical movement for enemy (move enemy car down).

enem1  ld a,(ix+1)         ; load enemy y into A.
       add a,espeed        ; add constant enemy vertical speed.
       ld (ix+1),a         ; update enemy y.
       ret

; --------------------------------------------------------------
; QUIET NOISE GENERATOR.

quiet  ld a,$ff            ; we want to kill the noise channel.
       out ($7f),a         ; write wish to psg port.
       ret

; --------------------------------------------------------------
; SET VDP REGISTER.
; Write to target register.
; A = byte to be loaded into vdp register.
; B = target register 0-10.

setreg out ($bf),a         ; output command word 1/2.
       ld a,$80
       or b
       out ($bf),a         ; output command word 2/2.
       ret

; --------------------------------------------------------------
; GET KEYS.
; Read player 1 keys (port $dc) into ram mirror (input).

getkey in a,$dc            ; read player 1 input port $dc.
       ld (input),a        ; let variable mirror port status.
       ret

; --------------------------------------------------------------
; MEMORY FILL.
; HL = base address, BC = area size, A = fill byte.

mfill  ld (hl),a           ; load filler byte to base address.
       ld d,h              ; make DE = HL.
       ld e,l
       inc de              ; increment DE to HL + 1.
       dec bc              ; decrement counter.
       ld a,b              ; was BC = 0001 to begin with?
       or c
       ret z               ; yes - then just return.
       ldir                ; else - write filler byte BC times,
                           ; while incrementing DE and HL...
       ret

; --------------------------------------------------------------
; SET COLOR.
; A = color index, B = color value (intensity).
setcol out ($bf),a
       ld a,%11000000
       out ($bf),a
       ld a,b
       out ($be),a
       ret

; --------------------------------------------------------------
; Wait for vertical blanking phase.
wait   ld a,(iflag)        ; load frame interrupt flag.
       or a                ; is it set?
       jp z,wait           ; no? - keep looping.
       xor a               ; else - reset flag.
       ld (iflag),a
       ret                 ; return.

; --------------------------------------------------------------
; DATA
; --------------------------------------------------------------
; Initial values for the 11 vdp registers.

regdat .db %00000110       ; reg. 0, display and interrupt mode.
                           ; bit 4 = line interrupt (disabled).
                           ; 5 = blank left column (disabled).
                           ; 6 = hori. scroll inhibit (disabled).
                           ; 7 = vert. scroll inhibit (disabled).

       .db %10100001       ; reg. 1, display and interrupt mode.
                           ; bit 0 = zoomed sprites (enabled).
                           ; 1 = 8 x 16 sprites (disabled).
                           ; 5 = frame interrupt (enabled).
                           ; 6 = display (blanked).

       .db $ff             ; reg. 2, name table address.
                           ; $ff = name table at $3800.

       .db $ff             ; reg. 3, n.a.
                           ; always set it to $ff.

       .db $ff             ; reg. 4, n.a.
                           ; always set it to $ff.

       .db $ff             ; reg. 5, sprite attribute table.
                           ; $ff = sprite attrib. table at $3F00.

       .db $ff             ; reg. 6, sprite tile address.
                           ; $ff = sprite tiles in bank 2.

       .db %11110011       ; reg. 7, border color.
                           ; set to color 3 in bank 2.

       .db $00             ; reg. 8, horizontal scroll value = 0.

       .db $00             ; reg. 9, vertical scroll value = 0.

       .db $ff             ; reg. 10, raster line interrupt.
                           ; turn off line int. requests.

; Initialization for hiscore and score in the sat buffer.

initsc .db 20 20 20 20
       .db 36 36 36 36
       .db 200 $30 208 $30 216 $30 224 $30
       .db 200 $30 208 $30 216 $30 224 $30

tshigh .db 151 151 151 151 $d0
       .db 150 0 166 0 182 0 198 0

; Charcodes for player, enemy and invisible car.

plrcar .db 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
crash .db 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
encar .db 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
invcar .db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

; Background assets.

bgpal  .include "assets\background (palette).inc"
blchar .include "assets\characters_blue (tiles).inc"
rdchar .include "assets\characters_red (tiles).inc"
bgtile .include "assets\background (tiles).inc"
bgmap  .include "assets\background (tilemap).inc"

; Sprite assets.

palspr .include "assets\sprites (palette).inc"
pltile .include "assets\player (tiles).inc"
entile .include "assets\enemy (tiles).inc"
plcras .include "assets\player_crashed (tiles).inc"

; Title screen assets.

timap  .include "assets\title (tilemap).inc"
titile .include "assets\title (tiles).inc"
titune .incbin "assets\RacerTitle_V02.psg"
.include "assets\PSGlib.inc"