; -------------------------------------------------------------;
;                      RACER                                   ;
; -------------------------------------------------------------;

.sdsctag 1.12, "Racer", "Speed and/or die!", "Anders S. Jensen"

.memorymap                 ; create 2 x 16 kb slots for rom.
       defaultslot 0
       slotsize $4000
       slot 0 $0000        ; rom bank 0 (0-16 kb).
       slot 1 $4000        ; rom bank 1 (16-32 kb).
       slotsize $2000
       slot 2 $c000        ; ram.
.endme

.rombankmap                ; map rom to 2 x 16 kb banks.
       bankstotal 2
       banksize $4000
       banks 2
.endro

.equ   vspeed 7            ; players' vertical speed.
.equ   hspeed 3            ; horiz. speed of player and enemy.
.equ   espeed 4            ; enemy's vertical speed.
.equ   rightb $8c          ; right border of the road.
.equ   leftb $14           ; left border of the road.

; Map of the sprite attribute table (sat) buffer.
; Contains sprites' vertical position (vpos), horizontal posi-
; tion (hpos) and character codes (cc).

.equ highvp $c000          ; first hiscore vpos.
.equ highhp $c080          ; first highscore hpos.
.equ highcc $c081          ; first hiscore cc.

.equ scorvp $c004          ; first score vpos.
.equ scorhp $c088          ; first score hpos.
.equ scorcc $c089          ; first score cc.

.equ plrvp $c008           ; first player vpos.
.equ plrhp $c090           ; first player hpos.
.equ plrcc $c091           ; first player cc.

.equ maevp $c018           ; first Mae vpos.
.equ maehp $c0b0           ; first Mae hpos.
.equ maecc $c0b1           ; first Mae cc.

.equ ashvp $c028           ; first Ash vpos.
.equ ashhp $c0d0           ; first Ash hpos.
.equ ashcc $c0d1           ; first Ash cc.

.equ endspr $c038          ; first unused sprite.

 ; Organize ram - create the sat buffer and variables.

.enum $c000 export         ; export labels to symbol file.

       satbuf dsb 256      ; sprite attribute table buffer.
                           ; see the map for object offsets.

       scroll db           ; vdp scroll register buffer.
       input db            ; input from player 1 controller.
       iflag db            ; frame interrupt flag.
       frame db            ; frame counter.
       status db           ; vdp status (for collision detect.).
       hscore dsb 4        ; 4 digits for hiscore 0000-9999.
       score dsb 4         ; 4 digits for score 0000-9999.
       record db           ; new record = hiscore is beaten!

       ashdir db           ; Ash's direction.
       ashy db             ; Ash y (vertical position).
       ashx db             ; Ash x (horizontal position).

       maedir db           ; Mae's direction.
       maey db             ; Mae y (vertical position).
       maex db             ; Mae x (horizontal position).
       maedel db           ; Mae delay.

       ply db              ; Player y.
       plx db              ; Player x.
.ende

.bank 0 slot 0
.org 0
       di                  ; disable interrupts.
       im 1                ; interrupt mode 1.
       ld sp,$dff0         ; default stack pointer address.
       jp inigam           ; initialize game.

; Read the vdp status flag at every frame interrupt.
; Sprite collision is read from bit 5 of the status flag.
; Set the frame interrupt flag.

.orga $0038                ; frame interrupt address. 
        ex af,af'           ; save accumulator in its shadow reg. 
        in a,$bf            ; get vdp status / satisfy interrupt. 
        ld (status),a       ; save vdp status in ram. 
        ld a,1              ; load accumulator with raised flag. 
        ld (iflag),a        ; set interrupt flag. 
        ex af,af'           ; restore accumulator. 
        ei                  ; enable interrupts.
        reti                ; return from interrupt. 

; Disable the pause button - this is an unforgiving game!

.orga $0066                ; pause button interrupt.
       retn                ; disable pause button.

; Initialize game.
; Overwrite the first 4 kb of ram with zeroes.

inigam ld hl,$c000         ; point to beginning of ram.
       ld bc,$1000         ; 4 kb to fill.
       ld a,0              ; with value 0.
       call mfill          ; do it!

; Use the initialized ram to clean all of vram.

       ld hl,$0000         ; prepare vram for data at $0000.
       call vrampr
       ld b,4              ; write 4 x 4 kb = 16 kb.
-      push bc             ; save the counter.
       ld hl,$c000         ; source = freshly initialized ram.
       ld bc,$1000         ; 4 kb of zeroes.
       call vramwr         ; purge vram.
       pop bc              ; retrieve counter.
       djnz -

 ; Initialize PSGLib.

       call PSGInit        ; credit goes to sverx!

; Load various assets into vram.

       ld hl,$c010         ; color bank 2, color 0 (sprites).
       call vrampr         ; prepare vram.
       ld hl,palspr        ; sprite palette data.
       ld bc,5             ; 5 colors.
       call vramwr         ; set sprite palette.

       ld hl,$2600         ; first tile @ index $130.
       call  vrampr        ; prepare vram at the above address.
       ld hl,blchar        ; the blue characters - for hiscore.
       ld bc,10*32         ; 10 tiles, each tile is 32 bytes.
       call vramwr         ; write blue character tils to vram.

       ld hl,$2800         ; first tile @ index $140.
       call  vrampr        ; prepare vram at the above address.
       ld hl,rdchar        ; the red characters - for score.
       ld bc,10*32         ; 10 tiles, each tile is 32 bytes.
       call vramwr         ; write red characters to vram.

       ld hl,$2000         ; first tile @ index 256.
       call vrampr         ; prepare vram.
       ld hl,pltile        ; player car tile data.
       ld bc,16*32         ; 16 tiles, 32 bytes each.
       call vramwr         ; write player car tiles to vram.

       ld hl,$2200         ; first tile @ index 272.
       call vrampr         ; prepare vram.
       ld hl,entile        ; enemy car tile data.
       ld bc,16*32         ; 16 tiles, 32 bytes each.
       call vramwr         ; write enemy car tiles to vram.

       ld hl,$2400         ; first tile @ index 288.
       call vrampr         ; prepare vram.
       ld hl,plcras        ; crashed player car data.
       ld bc,16*32         ; 16 tiles, 32 bytes each.
       call vramwr         ; write crashed player tiles to vram.

       ld hl,$c000         ; color bank 1, color 0.
       call vrampr         ; prepare vram.
       ld hl,bgpal         ; background palette.
       ld bc,4             ; 4 colors.
       call vramwr         ; set background palette.

; Initialize hiscore.

       ld hl,hscore+1      ; point to hundreds-digit in hiscore.
       ld b,2              ; we want to add 2 to this digit.
       call addsco         ; initialize hiscore counter to 0200.

; Initialize variables - once per game.

       xor a
       ld (maedir),a       ; Mae goes left.
       ld a,20             ;
       ld (ashx),a         ; set Ash x.
       ld a,100            ;
       ld (maex),a         ; set Mae x.
       ld a,140            ;
       ld (ply),a          ; set player y.

; Prepare for the title screen loop.

prepti

; Initialize the VDP registers.

       ld hl,regdat        ; point to register init data.
       ld b,11             ; 11 bytes of register data.
       ld c,$80            ; VDP register command byte.

-:     ld a,(hl)           ; load one byte of data into A.
       out ($bf),a         ; output data to VDP command port.
       ld a,c              ; load the command byte.
       out ($bf),a         ; output it to the VDP command port.
       inc hl              ; inc. pointer to next byte of data.
       inc c               ; inc. command byte to next register.
       djnz -              ; jump back to '-' if b > 0.

; Load title screen specific assets into vram.

       ld hl,$0000         ; first tile @ index 0.
       call vrampr         ; prepare vram.
       ld hl,titile        ; title screen tile data.
       ld bc,77*32         ; 77 tiles, each tiles is 32 bytes.
       call vramwr         ; write title screen tiles to vram.

       ld hl,$3800         ; point to name table.
       call vrampr         ; prepare vram.
       ld hl,timap         ; title screen tile map data.
       ld bc,32*24*2       ; 32 x 24 tiles, each is 2 bytes.
       call vramwr         ; write name table to vram.

; Set the hiscore y and x position.

       ld hl,tshigh        ; point to hiscore init data for title screen.
       ld de,highvp        ; target the (hi-) score sat buffer.
       ld bc,5             ; write 5 vpos bytes (incl. $d0).
       ldir                ; do it.
       ld hl,tshigh+5      ; point to hiscore init data.
       ld de,highhp        ; target buffer again.
       ld bc,8             ; write 8 hpos and cc bytes.
       ldir

; Create sprite data in the sat buffer for the hiscore sprites,
; and load the updated buffer into vram (screen is off now).

       call upbuf          ; update sat buffer.
       call ldsat          ; load sat in vram with buffer.

; Turn screen on.

       xor a
       ld b,9              ; reset scroll value.
       call setreg         ; set register 1.

       ld a,%11100001      ; enable screen + zoomed sprites.
       ld b,1              ; register 1.
       call setreg         ; set register.

       ei                  ; turn interrupts on.

; Play title screen tune

       ld hl,titune            ; load title screen tune
       call PSGPlayNoRepeat    ; play it once

; Display title screen and wait for player 1 button 1 press

-      halt                    ; start loop in vblank

; Prevent CRAM show on title screen (yes, ugly hack)

       ld b,200
delay  nop
       nop
       nop
       nop
       nop
       djnz delay

; Flash the hiscore

       ld hl,frame             ; point to the frame counter
       ld a,(hl)               ; load it into the accumulator
       and %11110000           ; apply quick and dirty bit masking
       or %00000011            ; to control the speed and color of
       ld b,a                  ; the flashing hiscore digits
       ld a,$14                ; index of color we want to change
       call setcol             ; set color of hiscore
       inc (hl)                ; increment counter


; Read and respond to controller input

; TODO remove TESTING
       ;jp ldmain
       call getkey             ; read controller port into ram
       ld a,(input)            ; read input from ram mirror
       bit 4,a                 ; is button 1 pressed
       jp z,ldmain             ; yes - load assets for main loop
       
       call PSGFrame           ; handle music
       jp -

; TODO remove TESTING

; Load assets for main loop.  Start with debouncing

ldmain  ld a,%10100000      ; turn display off
       ld b,1
       call setreg          ; do it by writing to register 1
       ld b,10              ; set up a 10 frame delay to prevent
-      halt                 ; bouncing keys
       djnz -
       
       call PSGStop         ; turn off the music
       di                   ; turn off interrupts

; Setup hiscore and score for main loop and remove terminator

       ld hl,initsc         ; point to (hi-)score init data
       ld de,highvp         ; target the (hi-) score sat buffer
       ld bc,8              ; write 8 vpos bytes
       ldir                 ; do it
       ld hl,initsc+8       ; point to (hi-)score init data #2
       ld de,highhp         ; target buffer again
       ld bc,16             ; write 16 hpos and cc bytes
       ldir

; Setup the background assets for the main loop

       ld hl,$0000          ; first tile 0 index 0
       call vrampr          ; prepare vram
       ld hl,bgtile         ; background tile data (the road)
       ld bc,2*32           ; 2 tiles (!), each tile is 32 bytes
       call vramwr          ; write background tiles to vram
       
       ld hl,$3800          ; point to name table
       call vrampr          ; prepare vram
       ld hl,bgmap          ; point to background tilemap data
       ld bc,32*28*2        ; 32 x 28 tiles, each is 2 bytes
       call vramwr          ; write name table to vram

; Make a standard enemy car for Ash and put it in the buffer

       ld de,ashcc          ; point to Ash char codes in buffer
       ld hl,encar          ; point to enemy car graphics
       call carcc           ; set Ash graphics to enemy car

; Adjust the sprite palette (player might be cyan)

       ld a,$11             ; player car's color
       ld b,%000010111      ; set it to default orange
       call setcol          ; do it
       
       ld a,$14             ; hiscore digit color
       ld b,%00110101       ; set them to default blue
       call setcol          ; do it

; Put a shining new player car in the buffer

       ld de,plrcc          ; point to player cc in buffer
       ld hl,plrcar         ; point to player car graphics
       call carcc           ; set the char codes for player car

; Make Mae invisible - put transparent tiles in the buffer

       ld de,maecc          ; point to Mae cc in buffer
       ld hl,invcar         ; point to transparent car graphics
       call carcc           ; make Mae invisible (transparent)
       
       call quiet           ; kill the noise generator

; Initialize variables after each crash

       ld a,50              ; Mae's initial y-coordinate
       ld (maey),a          ; set it
       ld a,230             ; Mae is invisible until delay = 0
       ld (maedel),a        ; set Mae invisibility delay
       ld a,193             ; Ash's initial y-coordiate
       ld (ashy),a          ; set it
       ld a,79              ; player starts at the road's center
       ld (plx),a           ; set x co-ordinate
       
       xor a                ; set A = 0
       ld (record),a        ; reset hiscore beaten flag
       ld (scroll),a        ; reset scroll register buffer
       ld (frame),a         ; reset frame counter
       
       ld hl,endspr         ; point to end of active sprites
       ld (hl),$d0          ; insert sprite terminator here

; Initialize score counter to 0000

       ld a,0               ; value to write to digit
       ld b,4               ; we have 4 digits
       ld hl,score          ; point to score data
-      ld (hl),a            ; reset digit (write value 0)
       inc hl               ; next digit
       djnz -               ; do it for all 4 digits
       
       call upbuf           ; update buffer for cars and digits
       
       ei                   ; enable frame interrupt (vblank)
       halt                 ; wait for it to happen
       call ldsat           ; load sat from buffer
       
       ld a,%11100000       ; turn screen on - normal sprites
       ld b,1
       call setreg          ; set register 1
       
       ld a,150             ; set 'back to title screen'-delay
       ld (frame),a         ; and load it into counter

; 'Ready loop' - wait until the player presses button 1
; The player's car is on the road, waiting for the button press
rloop  halt                 ; wait for the frame interrupt

; Jump back to title screen if the counter is depleted

       ld hl,frame          ; point to counter
       dec (hl)             ; decrement it
       jp z,prepti          ; is it 0? - Jump to title screen

; Proceed to main loop if the player presses start button

; TODO remove TESTING

       call getkey          ; read controller port into ram
       ld a,(input)         ; read input from ram mirror
       bit 4,a              ; is button 1 pressed?
       jp nz,rloop          ; yes - fall through to main loop!

; TODO remove TESTING

       ld a,%11110110       ; turn noise volume up to 12 db
       out ($7f),a          ; write to psg
       ld a,%11100110       ; make coarse noise - go car engine!
       out ($7f),a          ; write to psg

       xor a
       ld (iflag),a

; This is the main loop
mloop  call wait

; Update vdp right when vblank begins!

       ld a,(scroll)        ; 1-byte scroll reg. buffer in ram
       ld b,9               ; target VDP register 9 (v-scroll)
       call setreg          ; now vdp register = buffer, and the
                            ; screen scrolls accordingly

       call ldsat           ; load sat buffer to vram.  The cars
                            ; and the (hi-)score sprites are
                            ; updated on the screen

; Test for max score (9999) = player beats the game (and dies)!

       ld b,4               ; test up to 4 score digits
       ld hl,score          ; point to the first digit
-      ld a,(hl)            ; load it into accumulator
       cp 9                 ; is it 9?
       jp nz,+              ; no - then skip further tests
       inc hl               ; else - point to next digit
       djnz -               ; perform the test again
       jp plrdie            ; fall through = score is 9999!
+
; Test to see if current score > hiscore

       ld a,(record)        ; load pointer to new record flag
       cp 0                 ; is it set already (hiscore beaten)?
       jp z,score0          ; no - go on to test against score

; Hiscore is beaten, so let hiscore mirror score

       ld hl,score          ; point to score
       ld de,hscore         ; point to hiscore
       ld bc,$0004          ; 4 bytes (digits) to load
       ldir                 ; do it!
       jp endscr

score0 ld de,score          ; point to score
       ld hl,hscore         ; point to hiscore
       ld b,4               ; test 4 digits, from left to right

-      ld a,(de)            ; load score digit
       cp (hl)              ; is hiscore digit > score digit?
       jp c, endscr         ; yes - break out of loop
       inc hl               ; no - point to next hiscore digit
       inc de               ; point to next score digit
       djnz -               ; compare up to four digits

       ld a,l               ; fall through = new record is set!
       ld (record),a        ; set the flag, score > hiscore!

; Turn player's car and score bright blue!

       ld a,$11             ; color index of player's car
       ld b,%00111100       ; bright blue / cyan
       call setcol          ; set color to make a blue car
endscr:

; Test for collision between sprites

       ld a,(status)        ; load vdp status (set by interrupt)
       bit 5,a              ; is the sprite collision bit set?
       jp nz,plrdie         ; yes - jump out of main loop

; Test if player wants to move right

       call getkey          ; read controller port
       ld a,(input)         ; read input from ram mirror
       bit 3,a              ; is right key pressed?
       jp nz,mpl          ; no - test for left key

       ld a,(plx)            ; get player's hpos (x-coordinate)
       cp rightb            ; is player over the right border?
       jp nc,mpl            ; yes - skip to left test

; Move player right

       ld a,(plx)           ; get player x-coordinate
       add a,hspeed         ; add constant hspeed
       ld (plx),a           ; update player x-coordinate
       jp endchk            ; exit key check part

; Test if player wants to move left

mpl    ld a,(input)         ; read input from ram mirror
       bit 2,a              ; is left key pressed?
       jp nz,endchk         ; no - end key check

       ld a,(plx)           ; get player's hpos (x-coordinate)
       cp leftb             ; is player over the left border?
       jp c,endchk          ; yes - then don't move left
       
; Move player left

       ld a,(plx)           ; get player x-coordinate
       ld b,hspeed          ; load horizontal speed (constant)
       sub b                ; subtract hspeed from player x
       ld (plx),a           ; update player x-coordinate
endchk                      ; end key check

; Update enemy x,y positions

       ld ix,ashdir         ; point to enemy Ash data
       call enemy           ; move Ash down and left/right
       ld ix,maedir         ; point to enemy Mae data
       call enemy           ; move Mae down and left/right

; Handle Mae visibility

       ld a,(maedel)        ; check Mae delay
       cp $ff               ; is Mae already visible ($ff)?
       jp z,endmae          ; yes - then skip rest of Mae routine
       cp 0                 ; else - is delay counter deploeted?
       jp nz,+              ; no - then skip forward, else...

; Make Mae visible by loading opaque tiles into buffer

       ld hl,encar          ; point to enemy car char codes
       ld de,maecc          ; point to buffer
       call carcc           ; load new char codes to buffer

+      ld hl,maedel         ; point to Mae delay counter
       dec (hl)             ; decrement it
endmae

; Increment score 1 point every frame

       ld hl,score+3        ; point to the ones' digit in score
       ld b,1               ; load amount to add to digit
       call addsco          ; add one point to player's score

; Update enemy, player and score sprites in the buffer

       call upbuf           ; update sat buffer

; Scroll background - update the vertical scroll buffer

       ld a,(scroll)        ; get scroll buffer value
       sub vspeed           ; subtrat vertical speed
       ld (scroll),a        ; update scroll buffer

       jp mloop             ; jump back for another round

; When the player dies...

plrdie ld a,85              ; set death delay
       ld (frame),a         ; load it into frame counter

       ld a,%11110011       ; turn noise volume up to 6 db
       out ($7f),a          ; write to psg port
       ld a,%11100101       ; make medium coarse noise
       out ($7f),a          ; write to psg port

       ld hl,crash          ; point to crashed car graphics
       ld de,plrcc          ; point to player cc buffer start
       call carcc           ; set the player's sprite to crash!

; This is the death loop

dloop  call wait            ; start dead loop with vblank
       call ldsat           ; load sprite attribute table
       call upbuf           ; update the score

; Move enemy Ash fast upwards to create teh effect of enemy cars
; coming from behind when player is crashed

       ld a,(ashy)          ; load Ash vertical position
       cp 192               ; is Ash's y-coordinate = 192?
       jp z,+               ; yes - don't move him anymore
                            ; (he now hides below the screen)

       ld b,9               ; 9 pixels pr. frame is fast!
       ld a,(ashy)          ; load Ash's y-coordinate
       sub b                ; subtract from Ash's current y
       ld (ashy),a          ; load result back into variable
+
; Move enemy Mae fast upwards (just like we did with Ash above)

       ld a,(maey)
       cp 192               ; stop Mae in blanked screen area
       jp z,+               ; if she is not there, move her

       ld b,9               ; super fast at speed 9
       ld a,(maey)          ; load Mae's current y position
       sub b                ; subtract from Mae's current y
       ld (maey),a          ; load result back into variable
+
       ld a,(frame)         ; point to counter (death delay)
       cp 0                 ; is it 0?
       jp z,ldmain          ; yes - jump back and respawn player
       dec a                ; decrement the counter

; Check if we should kill the crash sound

       ld (frame),a
       ld a,(frame)         ; point to counter (death delay)
       cp 30                ; is it up?
       call z,quiet         ; yes - turn off crash sound

       jp dloop             ; another round of death...

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

bgpal  .include "Assets/background (palette).inc"

blchar .include "Assets/characters_blue (tiles).inc"

rdchar .include "Assets/characters_red (tiles).inc"

bgtile .include "Assets/background (tiles).inc"

bgmap  .include "Assets/background (tilemap).inc"


; Sprite assets.

palspr .include "Assets/sprites (palette).inc"

pltile .include "Assets/player (tiles).inc"

entile .include "Assets/enemy (tiles).inc"

plcras .include "Assets/player_crashed (tiles).inc"

; Title screen assets.

timap  .include "Assets/title (tilemap).inc"

titile .include "Assets/title (tiles).inc"

titune .incbin "Assets/RacerTitle_V02.psg"
.include "Assets/PSGlib.inc"