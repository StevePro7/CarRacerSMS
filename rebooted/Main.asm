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
.sdsctag 2.0, "Racer - rebooted", ReleaseNotes, "Anders S. Jensen"
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
    metasprite db       ; 2
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
   Ash INSTANCEOF EnemyObject   ; The gree enemy cars...
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