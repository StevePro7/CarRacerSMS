/****   R A C E R   ****
Racer - rebooted.
By Anders S. Jensen (2015).

This is a rebuilt and improved version of the classic Racer game from the 2015
SMS-Power! compo. I have used comments and constants to make the source code
more clear, but there is no tutorial. I consider Racer (classic) a stepping
stone towards Racer (rebooted). Will the source code scale into a much larger
and maybe even different game? I don't think so. Use the provided source on
your own risk, and feel free to ask about it in the forum. I'm still learning
abou this stuff myself.

Greetings to the community heroes for their tools and helpful assistance:
Maxim
Bock
sverx
Calindro

Music and jingles are purely homemade using Delek's Deflemask tracker, and
Racer is proud to be powered by sverx' PSGlib.

Titlescreen and celebration are hand-drawn, digitized images, converted with
Maxim's BMP2Tile. The racing car sprites are modified versions of sprites from
Super Monaco GP.
*/
.sdsctag 2.0, "Racer - rebooted", "ReleaseNotes", "Anders S. Jensen"
.memorymap
    defaultslot 0
    slotsize $8000
    slot 0 $0000            ; ROM
    slotsize $2000
    slot 1 $c000            ; RAM
.endme
.rombankmap
    bankstotal 1
    banksize $8000
    banks 1
.endro

; =============================================================================
; M A C R O S
; =============================================================================
.macro PrepareVram
    rst $20
.endm
.macro Outi_x4
    outi
    outi
    outi
    outi
.endm

; =============================================================================
; C O N S T A N T S
; =============================================================================
; VDP-related constants
.define    PALETTE_ADDRESS $c000 ; Bank 1 address.
.define    PALETTE_BANK_2 $c010 ; Bank 2 address.
.define    SPRITE_PALETTE_SIZE 16 ; 16 colors
.define    ONE_FULL_PALETTE 16
.define    START_OF_SPRITE_PALETTE 16
.define    SPRITE_COLLISION_BIT 5
.define    VDP_CONTROL $bf
.define    VDP_DATA $be
.define    VDP_INTERRUPT_ADDRESS $0038
.define    ALL_VDP_REGISTERS 11
.define    VDP_WRITE_REGISTER_COMMAND $80
.define    ADDRESS_OF_FIRST_TILE $0000
.define    TILEMAP_ADDRESS $3800
.define    SCORE_TILE_OFFSET 34 ; Used in the update score routine.
.define    SCORE_DIGITS_TILE_ADDRESS 34*32 ; racetrack is currently 34 tiles...
.define    SCORE_DIGITS_TILE_AMOUNT 20*32
.define    SCORE_DIGIT_1_ADDRESS $38f6
.define    TODAYS_BEST_SCORE_DIGIT_1_ADDRESS $3a76
.define    SAT_Y_TABLE $3f00
.define    SAT_XC_TABLE $3f80
.define    SPRITE_TERMINATOR $d0
.define    TURN_SCREEN_OFF %10000000
.define    TURN_SCREEN_ON_TALL_SPRITES %11100010
.define    VDP_REGISTER_1 1
.define    VDP_REGISTER_7 7
.define    SPRITE_COLOR_1 1
.define    VDP_VERTICAL_SCROLL_REGISTER 9
.define    WHOLE_NAMETABLE 32*28*2
.define    VISIBLE_PART_OF_SCREEN 32*24*2
.define    ORANGE $0b
.define    RED $03 ; not actually white... pff..
.define    DUMMY $23
; Player values
.define    PLAYER_VERTICAL_SPEED 6
.define    PLAYER_HORIZONTAL_SPEED 2 ;
.define    PLAYER_X_START 110
.define    PLAYER_Y_START 135
.define    FIRST_PLAYER_TILE $2800
.define    PLAYER_METASPRITE_SIZE 32*32
.define    PLAYER_HITCOUNTER_MAX 4
; Enemy values
.define    ASH_X_START 76
.define    ASH_Y_START 1
.define    MAY_X_START 30
.define    MAY_Y_START 85
.define    IRIS_X_START 90
.define    IRIS_Y_START 171
.define    ENEMY_HORIZONTAL_SPEED 1
.define    ENEMY_VERTICAL_SPEED 2
.define    FIRST_ENEMY_TILE $2400
.define    ENEMY_METASPRITE_SIZE 32*32
.define    PASSIVE_ENEMY 0
.define    GOING_RIGHT 0
.define    GOING_LEFT 1
.define    ENEMY_RIGHT_BORDER 140
.define    ENEMY_LEFT_BORDER 18
.define    EASY_MODE_MASK %00000111 ; Too easy/hard?!
.define    HARD_MODE_MASK %00000011
.define    HARD_MODE_THRESHOLD 3
; Misc
.define    STACK_INIT_ADDRESS $dff0
.define    PAUSE_INTERRUPT_ADDRESS $0066
.define    DEATH_DELAY 100
.define    GET_READY_DELAY 110
.define    MAX_ATTEMPTS 2 ; 0-2 = 3
.define    PLAYER1_JOYSTICK_RIGHT 3
.define    PLAYER1_JOYSTICK_LEFT 2
.define    PLAYER1_START 4
.define    BOTTOM_BORDER 193
.define    RIGHT_BORDER 156
.define    LEFT_BORDER 5
.define    MAX_CELS 2    ; Number of cels in a car's animation sequence.
.define    FLAG_UP 1
.define    FLAG_DOWN 0
.define    SCORE_LINE 135 ; When to score one point.
.define    TODAYS_BEST_SCORE_INITIAL_VALUE $0901 ; = 19.
.define    GAME_BEATEN_SCORE $0909 ; = 99.
.define    START_SCORE $0000 ; = 00.
.define    EASY_MODE 0
.define    HARD_MODE 1
.define    DISABLED 0
.define    ENABLED 1

.struct EnemyObject
    y db                ; 0 (ix+0)...
    x db                ; 1
    metasprite dw       ; 2
    cel db              ; 4
    index db            ; 5
    movement db         ; 6
    status db           ; 7
.endst
; =============================================================================
; V A R I A B L E S
; =============================================================================
.ramsection "Game variables" slot 1
   VDPStatus db          ; Gets updated by the frame int. handler.
   FrameCounter db       ; Part of housekeeping.
   Joystick1 db          ; For input from the joystick ports
   Joystick2 db          ; (via the ReadJoysticks function).
   SpriteBufferY dsb 64  ; The 64 y-positions.
   SpriteBufferXC dsb 128 ; The 64 x-position + character code pairs.
   ScoreBuffer dsb 8     ; Two score digits take up 4 name table words.
   TodaysBestScoreBuffer dsb 8 ; Same with todays best score...
   Score dw              ; Two bytes, one for each digit.
   TodaysBestScore dw    ; The same...
   NewBestScoreFlag db   ; Is the current score todays best score?
   Scroll db             ; Vertical scroll register mirror.
   CollisionFlag db      ; Collision occured.
   GameBeatenFlag db     ; Is the score overflowing from 99?
   RandomSeed dw         ; For the randomize routine.
   PlayerY db            ; Player's y-position (metasprite).
   PlayerX db            ; Players x-position (metasprite).
   PlayerMetaSpriteDataPointer dw ; Pointer to metasprite data.
   PlayerCel db          ; Whih animation cel is currently playing?
   PlayerIndex db        ; Which SAT buffer slot does the player occupy?
   PlayerHitCounter db   ; Player can slightly touch the enemies...
   Ash INSTANCEOF EnemyObject   ; The three enemy cars...
   May INSTANCEOF EnemyObject
   Iris INSTANCEOF EnemyObject
   GameModeCounter dw    ; Counting up to the hard mode threshold.
   GameMode db           ; Is it easy or hard? (% enemies w. horiz. move).
   AttemptCounter db     ; Number of attempts before going back to title.
   Cycle db              ; Used by the color cycle routine at the title.
   Counter db            ; Used for title animation.
.ends

; =============================================================================
; L I B R A R I E S
; =============================================================================
.include "Support/stdlib.inc"   ; General/supporting routines.
.include "Support/PSGlib.inc"   ; sverx's psg library.

; =============================================================================
; R O M
; =============================================================================
.bank 0 slot 0
.org 0
    di
    im 1
    ld sp,STACK_INIT_ADDRESS
    jp Control
; ---------------------
.org $0020              ; rst $20: Prepare vram at address in HL.
    ld a,l              ; Refer to the PrepareVram macro.
    out (VDP_CONTROL),a
    ld a,h
    or $40
    out (VDP_CONTROL),a
    ret
; ---------------------
.org VDP_INTERRUPT_ADDRESS
    ex af,af'
    exx
    in a,VDP_CONTROL
    ld (VDPStatus),a
    exx
    ex af,af'
    ei
    ret
; ---------------------
.org PAUSE_INTERRUPT_ADDRESS
    retn
; ---------------------
.section "Main control structure" free
Control:
    call InitializeFramework
ShowTitleScreen:
    call PrepareTitlescreen
    call TitlescreenLoop
    xor a
    ld (AttemptCounter),a
    

    
dead:
    jp dead
    
.ends
; ---------------------
.section "Initialize" free
InitializeFramework:
    call ClearRam
    call PSGInit
    ld hl,RegisterInitValues
    call LoadVDPRegisters
    call SetHighScores
    ret
SetHighScores:
    ld hl,TODAYS_BEST_SCORE_INITIAL_VALUE
    ld (TodaysBestScore),hl
    ret
.ends
; ---------------------
.section "Titlescreen" free
PrepareTitlescreen:
    di
    call PSGSFXStop
    call PSGStop
    ld a,TURN_SCREEN_OFF
    ld b,VDP_REGISTER_1
    call SetRegister
    call LoadTitleScreen
    ld a,TURN_SCREEN_ON_TALL_SPRITES
    ld b,VDP_REGISTER_1
    call SetRegister
    ld hl,Intro
    call PSGPlayNoRepeat
    ei
    ret
LoadTitleScreen:
    ld hl,SAT_Y_TABLE
    PrepareVram
    ld c,VDP_DATA
    ld a,SPRITE_TERMINATOR
    out (c),a               ; Kill the sprites.
    ld a,0
    ld b,VDP_VERTICAL_SCROLL_REGISTER
    call SetRegister
    ld ix,TitlescreenImageData
    call LoadImage
    ret
TitlescreenLoop:
    call WaitForFrameInterrupt
    call AnimateTitle
    call PSGFrame
    call Housekeeping
    ld a,(Joystick1)
    bit PLAYER1_START,a
    ret z
    jp TitlescreenLoop
AnimateTitle:
    ld a,(Counter)          ; Is it time for a color cycle...?
    add a,64
    ld (Counter),a
    cp 0
    call z,DoColorCycle
    ret
DoColorCycle:
    ld c,VDP_CONTROL        ; Prepare vram at the correct color in the palette.
    ld a,$06
    out (c),a
    or %11000000
    out (c),a
    ld c,VDP_DATA
    ld d,0
    ld a,(Cycle)
    inc a
    cp 6                    ; Each letter + one all orange.
    jp nz,+
    xor a
+:
    ld (Cycle),a
    add a,a
    add a,a
    add a,a                 ; Cycle counter times 8,
    ld e,a                  ; - it's an 8 byte element table here!
    ld hl,PaletteTable
    add hl,de               ; Apply the offset.
    Outi_x4
    Outi_x4
    ret
.ends  
; ---------------------
.section "Misc functions" free
Housekeeping:
    call ReadJoysticks
    call IncrementFrameCounter
    ld hl,RandomSeed        ; Modify LSB of seed every frame.
    inc (hl)
    ret
IncrementFrameCounter:
    ld hl,FrameCounter
    inc (hl)
    ret
.ends
; ---------------------
.section "VDP functions" free
LoadVDPRegisters:
    ld b,ALL_VDP_REGISTERS
    ld c,VDP_WRITE_REGISTER_COMMAND
-:  ld a,(hl)               ; HL = Pointer to 11 bytes of data.
    out (VDP_CONTROL),a
    ld a,c
    out (VDP_CONTROL),a
    inc hl
    inc c
    djnz -
    ret
LoadImage:
    ld hl,ADDRESS_OF_FIRST_TILE
    PrepareVram
    ld l,(ix+0)             ; Load pointer to first tile into HL.
    ld h,(ix+1)
    ld c,(ix+2)             ; Load amount of tiles into BC.
    ld b,(ix+3)
    call LoadVRAM
    ld hl,TILEMAP_ADDRESS   ; Load tilemap.
    PrepareVram
    ld l,(ix+4)             ; Load pointer to first tilemap word into HL.
    ld h,(ix+5)
    ld c,(ix+6)             ; Amount of bytes to load.
    ld b,(ix+7)
    call LoadVRAM
    ld hl,PALETTE_ADDRESS
    PrepareVram
    ld l,(ix+8)             ; Load pointer to palette data into HL.
    ld h,(ix+9)
    ld c,(ix+10)            ; Load amount of colors into BC.
    ld b,(ix+11)
    call LoadVRAM
    ret
LoadSAT:
    ld hl,SAT_Y_TABLE
    PrepareVram
    ld hl,SpriteBufferY
    ld c,VDP_DATA
    call Outi_64
    ld hl,SAT_XC_TABLE
    PrepareVram
    ld hl,SpriteBufferXC
    ld c,VDP_DATA
    call Outi_128
    ret
LoadNameTable:
    ld hl,SCORE_DIGIT_1_ADDRESS
    PrepareVram
    ld hl,ScoreBuffer
    ld c,VDP_DATA
    Outi_x4
    ld hl,SCORE_DIGIT_1_ADDRESS+64
    PrepareVram
    ld hl,ScoreBuffer+4
    Outi_x4

    ld hl,TODAYS_BEST_SCORE_DIGIT_1_ADDRESS
    PrepareVram
    ld hl,TodaysBestScoreBuffer
    ld c,VDP_DATA
    Outi_x4
    ld hl,TODAYS_BEST_SCORE_DIGIT_1_ADDRESS+64
    PrepareVram
    ld hl,TodaysBestScoreBuffer+4
    Outi_x4
    ret
.ends
; ---------------------
.section "Data" free
Engine:
   .incbin "Race\Engine.psg"
Engine2:
   .incbin "Race\Engine2.psg"
Crash:
   .incbin "Race\Crash.psg"
NewBestScoreSFX:
   .incbin "Race\NewBestScore.psg"
Intro:
   .incbin "Race\Intro.psg"
CelebrateSound:
   .incbin "Race\Celebrate.psg"

PaletteTable:            ; Used to color-cycle the title letters.
   .db ORANGE ORANGE ORANGE ORANGE ORANGE DUMMY DUMMY DUMMY
   .db RED ORANGE ORANGE ORANGE ORANGE DUMMY DUMMY DUMMY
   .db ORANGE RED ORANGE ORANGE ORANGE DUMMY DUMMY DUMMY
   .db ORANGE ORANGE RED ORANGE ORANGE DUMMY DUMMY DUMMY
   .db ORANGE ORANGE ORANGE RED ORANGE DUMMY DUMMY DUMMY
   .db ORANGE ORANGE ORANGE ORANGE RED DUMMY DUMMY DUMMY

PlayerCel0:
   .db 0 0 0 0 16 16 16 16 ; Y-offset.
   .db 0 64 8 66 16 68 24 70 0 72 8 74 16 76 24 78 ; X-offset + char pairs.
PlayerCel1:
   .db 0 0 0 0 16 16 16 16
   .db 0 88 8 66 16 68 24 90 0 92 8 74 16 76 24 94
PlayerCel2:
   .db 0 0 0 0 16 16 16 16
   .db 0 80 8 66 16 68 24 82 0 84 8 74 16 76 24 86
PlayerCelTable:
   .dw PlayerCel0
   .dw PlayerCel1
   .dw PlayerCel2

; Enemies
DisabledCar:             ; Character 0 is totally transparent.
   .db 0 0 0 0 0 0 0 0
   .db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
GreenCarCel0:
   .db 0 0 0 0 16 16 16 16 ; Y-offset.
   .db 0 32 8 34 16 36 24 38 0 40 8 42 16 44 24 46 ; X-offset + chars.
GreenCarCel1:
   .db 0 0 0 0 16 16 16 16
   .db 0 56 8 34 16 36 24 58 0 60 8 42 16 44 24 62
GreenCarCel2:
   .db 0 0 0 0 16 16 16 16
   .db 0 48 8 34 16 36 24 50 0 52 8 42 16 44 24 54
EnemyCelTable:
   .dw GreenCarCel0
   .dw GreenCarCel1
   .dw GreenCarCel2
RespawnTable:            ; X-positions for enemy respawn.
   .db 20 40 60 80 100 120 140 150
   .db 25 43 63 83 92 102 119 145
   .db 28 44 64 108 126 140 98 123 142
   .db 30 46 66 86 140 84 95 105

RacetrackTiles:
   .include "Race\rt_tiles.inc"
RacetrackTilesEnd:
RacetrackTilemap:
   .include "Race\rt_tilemap.inc"
RacetrackPalette:
   .include "Race\rt_palette.inc"
RacetrackPaletteEnd:
PlayerCar_Tiles:
   .include "Race\PlayerCar_tiles.inc"
ScoreDigits_Tiles:
   .include "Race\ScoreDigits_tiles.inc"
EnemyCar_Tiles:
   .include "Race\EnemyCar_tiles.inc"
Sprites_Palette:
   .include "Race\Sprites_palette.inc"
RacetrackMockupData:
   .dw RacetrackTiles    ; Pointer to tile data.
   .dw RacetrackTilesEnd-RacetrackTiles ; Tile data (bytes) to write.
   .dw RacetrackTilemap ; Pointer to tilemap data.
   .dw WHOLE_NAMETABLE  ; Overwrite the whole nametable.
   .dw RacetrackPalette ; Pointer to palette.
   .dw RacetrackPaletteEnd-RacetrackPalette ; Amount of colors.

TitlescreenTiles:
   .include "Title\Titlescreen_tiles.inc"
TitlescreenTilesEnd:
TitlescreenTilemap:
   .include "Title\Titlescreen_tilemap.inc"
TitlescreenPalette:
   .include "Title\Titlescreen_palette.inc"
TitlescreenPaletteEnd:
TitlescreenImageData:
   .dw TitlescreenTiles  ; Pointer to tile data.
   .dw TitlescreenTilesEnd-TitlescreenTiles ; Tile data (bytes) to write.
   .dw TitlescreenTilemap ; Pointer to tilemap data.
   .dw VISIBLE_PART_OF_SCREEN ; Amount of bytes to write to tilemap.
   .dw TitlescreenPalette ; Pointer to palette.
   .dw TitlescreenPaletteEnd-TitlescreenPalette ; Amount of colors.

CelebrationscreenTiles:
   .include "Celebrate\celebrate_tiles.inc"
CelebrationscreenTilesEnd:
CelebrationscreenTilemap:
   .include "Celebrate\celebrate_tilemap.inc"
CelebrationscreenPalette:
   .include "Celebrate\celebrate_palette.inc"
CelebrationscreenPaletteEnd:
CelebrationscreenImageData:
   .dw CelebrationscreenTiles  ; Pointer to tile data.
   .dw CelebrationscreenTilesEnd-CelebrationscreenTiles ; Tile data (bytes) to write.
   .dw CelebrationscreenTilemap ; Pointer to tilemap data.
   .dw VISIBLE_PART_OF_SCREEN ; Amount of bytes to write to tilemap.
   .dw CelebrationscreenPalette ; Pointer to palette.
   .dw CelebrationscreenPaletteEnd-CelebrationscreenPalette ; Amount of colors.

RegisterInitValues:
   .db %10000110         ; reg. 0, display and interrupt mode.
                         ; bit 3 = shift sprites to the left (disabled).
                         ; 4 = line interrupt (disabled - see register 10).
                         ; 5 = blank left column (disabled).
                         ; 6 = hori. scroll inhibit (disabled).
                         ; 7 = vert. scroll inhibit (enabled).
   .db %11100000         ; reg. 1, display and interrupt mode.
                         ; bit 0 = zoomed sprites (disabled).
                         ; 1 = 8 x 16 sprites (disabled).
                         ; 5 = frame interrupt (enabled).
                         ; 6 = display (enabled).
   .db $ff               ; reg. 2, name table address.
                         ; $ff = name table at $3800.
   .db $ff               ; reg. 3, n.a. (always set it to $ff).
   .db $ff               ; reg. 4, n.a. (always set it to $ff).
   .db $ff               ; reg. 5, sprite attribute table.
                         ; $ff = sprite attrib. table at $3F00.
   .db $ff               ; reg. 6, sprite tile address.
                         ; $ff = sprite tiles in bank 2.
   .db %11110000         ; reg. 7, border color.
                         ; set to color 0 in bank 2.
   .db $00               ; reg. 8, horizontal scroll value = 0.
   .db $00               ; reg. 9, vertical scroll value = 0.
   .db $ff               ; reg. 10, raster line interrupt. (disabled).
ReleaseNotes:
   .db "...." 0
.ends
; ---------------------
.section "Outiblock" free
Outi_128:                ; Used to fastload SAT XC.
   .rept 64
   outi
   .endr
Outi_64:                 ; Used to fastload SAT Y.
   .rept 64
   outi
   .endr
   ret
.ends