; -------------------------------------------------------------;
;                      RACER                                   ;
; -------------------------------------------------------------;

.sdsctag 1.12, "Racer", "Speed and/or die!", "Anders S. Jensen"

.memorymap              ; create 2 x 16 bk slots for rom
    defaultslot 0
    slotsize $4000
    slot 0 $0000        ; rom bank 0 (0-16 kb).
    slot 1 $4000        ; rom bank 1 (16-32 kb).
    slotsize $2000
    slot 2 $c000        ; ram
.endme

.rombankmap             ; map rom to 2 x 16 bk banks
    bankstotal 2
    banksize $4000
    banks 2
.endro

.equ    vspeed 7        ; players' vertical speed
.equ    hspeed 3        ; horiz. speed of player and enemy
.equ    espeed 4        ; enemy's vertical speed
.equ    rightb $8c      ; right border of the road
.equ    leftb  $14      ; left border of the road

; Map of the sprite attribte talbe (sat) buffer
; Contains sprites' vertifcal position (vpos), horizontal posi-
; tion (hpos) and character codes (cc)

.equ    highvp  $c000   ; first hiscore vpos
.equ    highhp  $c080   ; first hiscore hpos
.equ    highcc  $c081   ; first hiscore cc

.equ    scorvp  $c004   ; first score vpos
.equ    scorhp  $c088   ; first score hpos
.equ    scorcc  $c089   ; first score cc

.equ    plrvp   $c008   ; first player vpos
.equ    plrhp   $c090   ; first player hpos
.equ    plrcc   $c091   ; first player cc

.equ    maevp   $c018   ; first Mae vpos
.equ    maehp   $c0b0   ; first Mae hpos
.equ    maecc   $c0b1   ; first Mae cc

.equ    ashvp   $c028   ; first Ash vpos
.equ    ashhp   $c0d0   ; first Ash hpos
.equ    ashcc   $c0d1   ; first Ash cc

.equ    endspr  $c038   ; first unused sprite

; Organize ram - create the sat buffer and variables

.enum $c000 export      ; export labels to symbol file

    satbuf  dsb 256     ; sprite attribute table buffer
                        ; see map for object offsets

    scroll  db          ;  vdp scroll register buffer
    input   db          ; input from player 1 controller
    iflag   db          ; frame interrupt flag
    frame   db          ; frame counter
    status  db          ; vdp status (for collision detect)
    hscore  dsb 4       ; 4 digits for hiscore  000-9999
    score   dsb 4       ; 4 digits for score    000-9999
    record  db          ; new record = hiscore is beaten!

    ashdir  db          ; Ash's direction
    ashy    db          ; Ash y (vertical position)
    ashx    db          ; Ash x (horizontal position)

    maedir  db          ; Mae's direction
    maey    db          ; Mae y (vertical position)
    maex    db          ; Mae x (horizontal position)

    ply     db          ; Player y
    plx     db          ; Player x
.ende

.bank 0 slot 0
.org 0
    di                  ; disable interrupts
    im 1                ; interrupt mode 1
    jp inigam           ; initialize game

; Read the vdp status flag at every frame interrupt
; Sprite collision is read from bit  of the status flag
; Set the frame interrupt flag.

.orga $0038             ; frame interrupt address
    ex af,af'           ; save accumulator in its shadow reg. 
    in a,$bf            ; get vdp status / satisfy interrupt. 
    ld (status),a       ; save vdp status in ram. 
    ld a,1              ; load accumulator with raised flag. 
    ld (iflag),a        ; set interrupt flag. 
    ex af,af'           ; restore accumulator. 
    ei                  ; enable interrupts.
    reti                ; return from interrupt. 

.orga $0066             ; pause button interrupt.
    retn                ; disable pause button

; Initialize game.
; Overwrite the first 4 kb of ram with zeroes

inigam:
    ld hl,$c000         ; point to beginning of ram
    ld bc,$1000         ; 4 kb to fill
    ld a,0              ; with value 0
    call mfill          ; do it!

; Use the initialized ram to clean all of vram

    ld hl,$0000         ; prepare vram for data at $0000
    call vrampr
    ld b,4              ; write 4 x 4 kb = 16 kb
    push bc             ; save the counter
    ld hl,$c000         ; source = freshly initialized ram
    ld bc,$1000         ; 4 kb of zeros
    call vramwr         ; purge vramwr
    
    
    
    
loop:
    jp loop



; --------------------------------------------------------------
; SUBROUTINES
; --------------------------------------------------------------
; PREPARE VRAM
; Set up vdp to receive data at vram address in HL

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
; WRITE TO RAM
; Write BC amount of bytes from data source pointed to by HL
; Tip: Use vrampr before calling

vramwr:
    ld a,(hl)
    out ($be),a
    inc hl
    dec bc
    ld a,c
    or b
    jp nz,vramwr
    ret

; --------------------------------------------------------------
; LOAD SPRITE ATTRIBUTE TABLE
; Load data into sprite attribute table (SAT) from the buffer

ldsat:
    ld hl,$3f00;        ; point to the start of SAT in vram
    call vrampr         ; prepare vram to receive data
    ld b,255            ; amount of bytes to output
    ld c,$be            ; destination is vdp data port
    ld hl,satbuf        ; source is start of sat buffer
    otir                ; output buffer to vdp
    ret

; --------------------------------------------------------------
; UPDATE SAT BUFFER
; Generate vpos, hpos, and cc data for the sprites that make up
; each of the cars (player, Mae and Ash)
; Also generate char code (cc) data from hiscore and score

; Generate sat buffer data from player's x,y coordinates

upbuf:
    ld a,(ply)          ; load player's current y co-ordinate
    ld hl,plrvp         ; point to sat buffer
    call cary           ; refresh buffer according to y




; --------------------------------------------------------------
; CAR Y TO SPRITES' VERTICAL POSITIONS (VPOS) IN BUFFER
; Generate vpos sat buffer data from a car's y position
; A = car's y (i.e. ply), HL = buffer address of car vpos

cary:
    ld b,4              ; a car is 4 tiles wide
-:
    push af             ; a row of 4 tiles share the same y
    push af             ; so here the y's are saved on stack
    push af
    push af
    add a,8             ; next row is 8 pixels below
    djnz -

; --------------------------------------------------------------
; CAR X TO SPRITES' HORIZONTAL POSITIONS (HPOS) IN BUFFER
; Generates hpos sat buffer data from a car's x position
; A = car's x (i.e. plx), HL = buffer address of car _high_part_Tone.section
carx:
    ld c,a              ; save hpos in c
    .rept 4             ; wladx: Repeat code four times
    ld a,c              ; load hpos into A
    ld b,4              ; loop: Repeat four times
-:
    ld (hl),a           ; write value to buffer at address
    inc hl              ; skip over the char code byte
    inc hl              ; point to next hpos byte in buffer
    add a,8             ; add 8 (a tile's width in pixels)
    djnz -              ; jump back
    .endr               ; end of xladx repeat directive
    ret

; --------------------------------------------------------------
; SET CAR SPRITES' CHARACTER CODES (CC)
; HL = pointer to 16 byte char codes block, DE = buffer index

carcc:
    ld bc,16
-:
    ldi    
    inc de
    ld a,b
    or c
    jp nz,-
    ret

; --------------------------------------------------------------
; ADD TO SCORE
; Add points to the score
; HL = address of the digit we want to increase
; B = the amount by which to increase the digit

addsco:
    ld a,(hl)               ; get the value of the digit
    add a,b                 ; add the amount to this value
    ld (hl),a               ; put updated digit back in string
    cp 9                    ; test updated digit
    ret c                   ; if 9 or less, relax and return
    ret z
    
; Update the next digit to the left

    sub 10                  ; make digit '0'
    ld (hl),a               ; and load it into position
-:
    dec hl                  ; move pointer to next digit (left)
    inc (hl)                ; increase that digit
    ld a,(hl)               ; load value into a
    cp 9                    ; test it like before
    ret c                   ; if 9 or less then scoring is done
    ret z                   ;
    sub 10                  ; else - make digit '0'
    ld (hl),a               ; and load it into position
    jp -                    ; update and test next digit

; --------------------------------------------------------------
; UPDATE ENEMY
; Calculate new x,y positions for an enemy car
; IX = start of enemy data block

enemy:
    ld a,(ix+0)             ; test direction
    cp 0                    ; moving left (0=left, 1=right)?
    jp nz,enem0             ; no - then enemy is moving right
    
; Direction: Left - test left border

    ld a,(ix+2)             ; load enemy's x-coordinate
    cp leftb                ; compare it to left border constant
    jp nc,+                 ; branch if accumulator (x) > leftb
                            ; else - enemy is on the left border
    ld a,1                  ; shift direction to 'right'
    ld (ix+0),a             ; load it into direction byte
    jp enem1                ; skip forward to vertical movement

; Direction Left - subtract from enemy x coordinate

+:
    ld b,hspeed             ; load horizontal speed into b
    ld a,(ix+2)             ; load enemy x into a
    sub b                   ; subtract hspeed from x (move left)
    ld (ix+2),a             ; update enemy x coordinate
    jp enem1                ; skip forward to vertical movement
    
; Direction: Right - test right border

enem0:
    ld a,(ix+2)             ; load enemy xl
    cp rightb               ; compare it to right border
    jp c,+                  ; skip if rightb > accumulator (x)
    
    xor a                   ; else - shift direction to 0 = left
    ld (ix+0),a             ; load new value into direction var
    jp enem1                ; forward to vertical movement
    
; Direction: Right - add to enemy x coordiate

+:
    ld b,hspeed             ; load hspeed constant into b
    ld a,(ix+2)             ; load enemy x into a
    add a,b                 ; add hspped to enemy x (move right)
    ld (ix+2),a             ; update enemy x coordinate
    
; Vertical movement for enemy (move enemy car down)

enem1:
    ld a,(ix+1)             ; load enemy y into a
    add a,espeed            ; add constant enemy vertical speed
    ld (ix+1),a             ; update enemy y
    ret    

; --------------------------------------------------------------
; QUIET NOISE GENERATOR

quiet:
    ld a,$ff                ; we want to kill the noise channel
    out ($7f),a             ; write wish to psg port
    ret
    
; --------------------------------------------------------------
; SET VDP REGISTER
; Write to target register
; A = byte to be loaded into vdp register
; B = target register 0-10

setreg:
    out ($bf),a             ; output command word 1/2
    ld a,$80
    or b
    out ($bf),a             ; output command work 2/2
    ret

; --------------------------------------------------------------
; GET KEYS
; Read player 1 keys (port $dc) into ram mirror (input)

getkey:
    in a,$dc                ; read player 1 input port $dc
    ld (input),a            ; let varialbe mirror port PSGGetStatus
    ret

; --------------------------------------------------------------
; MEMORY FILL
; HL = base address, BC = area size, A = fill byte

mfill:
    ld (hl), a          ; load filler byte to base address
    ld d,h              ; make DE = HL
    ld e,l
    inc de              ; increment DE to HL + 1
    dec bc              ; decrement counter
    ld a,b              ; was BC = 0001 to begin with?
    or c
    ret z               ; yes - then just return
    ldir                ; else - write filler byte BC times
                        ; while incrementing HE and HL...
    ret