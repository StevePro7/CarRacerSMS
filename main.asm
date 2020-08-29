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

loop:
    jp loop