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
   Ash INSTANCEOF EnemyObject ; The three enemy cars...
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
Racetrack:
   call PrepareRace
   call GetReady
   call MainLoop
   call Death
   ld a,(NewBestScoreFlag)          ; Is today's best score beaten?
   cp FLAG_UP
   jp nz,+
   call Celebrate                   ; Show celeb screen before going back to title.
   jp ShowTitleScreen
+:
   ld a,(GameBeatenFlag)            ; Is the game beaten?
   cp FLAG_UP
   jp nz,+
   call Celebrate                   ; This calls for celebration...
   jp ShowTitleScreen
+:
   ld a,(AttemptCounter)            ; Are we out of attempts to beat todays hiscore?
   cp MAX_ATTEMPTS
   jp nz,+
   xor a
   ld (AttemptCounter),a
   jp ShowTitleScreen
+:
   inc a                            ; If none of the above, fall through to here and
   ld (AttemptCounter),a            ; have another go at the race track.
   jp Racetrack
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
.section "Racetrack code" free
PrepareRace:
   di
   ld a,TURN_SCREEN_OFF
   ld b,VDP_REGISTER_1
   call SetRegister
   call InitializeGeneralVariables
   call PSGStop
   call PSGSFXStop
   call InitializeBackground
   call InitializeScore
   call InitializeSprites
   call UpdateSATBuffers
   call UpdateNameTableBuffers
   call LoadSAT            ; Load the sprite attrib. table from the buffers.
   call LoadNameTable
   ld a,SPRITE_COLOR_1     ; Color the border
   ld b,VDP_REGISTER_7
   call SetRegister
   ld a,TURN_SCREEN_ON_TALL_SPRITES
   ld b,VDP_REGISTER_1
   call SetRegister
   ei
   halt                    ; Make sure yo don't die right when race restarts.
   halt
   ret
InitializeScore:
   ld hl,SCORE_DIGITS_TILE_ADDRESS
   PrepareVram
   ld hl,ScoreDigits_Tiles
   ld bc,SCORE_DIGITS_TILE_AMOUNT
   call LoadVRAM
   ret
InitializeBackground:
   ld ix,RacetrackMockupData     ; Load the racetrack dummy image data.
   call LoadImage
   ret
InitializeSprites:
   ld hl,PALETTE_BANK_2          ; Load the sprites palette.
   PrepareVram
   ld hl,Sprites_Palette
   ld bc,ONE_FULL_PALETTE
   call LoadVRAM
   call InitializePlayer
   call InitializeEnemies
   ret
InitializeGeneralVariables:
   xor a
   ld (CollisionFlag),a
   ld (GameBeatenFlag),a
   ld (NewBestScoreFlag),a
   ld a,r
   ld (RandomSeed),a
   ld a,EASY_MODE
   ld (GameMode),a
   ld hl,START_SCORE          ; Put a zero in both of the player's score digits.
   ld (Score),hl
   ret
GetReady:                     ; Wait a little before the action starts.
   ld hl,Engine
   call PSGSFXPlay            ; Play the hrmmm.. hrm.... hrrrrmmmm. sound.
   ld b,GET_READY_DELAY
-:
   halt
   push bc
   call PSGSFXFrame           ; Wait while the sound effects plays.
   pop bc
   djnz -
   ld hl,Engine2              ; Shift to regular low-pitch motor sound.
   call PSGSFXPlayLoop
   ret
; ---------------------
MainLoop:
   call WaitForFrameInterrupt
   call DetectCollision       ; Set CollisionFlag if two hardware sprites overlap.
   ld a,(CollisionFlag)       ; Respond to collision flag.
   cp FLAG_UP                 ; Return to control loop upon collision.
   ret z
   call LoadSAT               ; Load Sprite Attrib. Table from the buffers.
   ld a,(Scroll)
   ld b,VDP_VERTICAL_SCROLL_REGISTER
   call SetRegister           ; Load the vertical scroll register.
   call LoadNameTable         ; Load selected parts of the name table (the score).
   call Housekeeping          ; General housekeeping.
   call HandleBestScore       ; Respond to score > todays best score.
   ld a,(GameBeatenFlag)      ; Return to control loop if score is 99 + 1.
   cp FLAG_UP
   ret z
   call HandleGameModeCounter ; Is it time to switch to hard mode yet?
   call MovePlayer            ; Move the player car.
   call MoveEnemies           ; Move the three enemies.
   call ScrollRacetrack       ; Update the scroll register buffer.
   call AnimatePlayer         ; The player car has three animation cels (wheels).
   call AnimateEnemies        ; ... and so does the enemies.
   call UpdateSATBuffers      ; Compute and buffer the hardware sprites.
   call UpdateScoreBuffer     ; Compute and buffer the name table elements.
   call UpdateTodaysBestScoreBuffer ;  ...
   call PSGFrame              ; Play music.
   call PSGSFXFrame           ; Play sound effects.
   jp MainLoop                ; Do it all again...
DetectCollision:
   ld a,(VDPStatus)           ; Check for sprite collision.
   bit SPRITE_COLLISION_BIT,a
   jp nz,+
   xor a
   ld (PlayerHitCounter),a
   ret
+:
   ld a,(PlayerHitCounter)
   inc a
   ld (PlayerHitCounter),a
   cp PLAYER_HITCOUNTER_MAX
   ret nz
   ld a,FLAG_UP
   ld (CollisionFlag),a
   ret                        ; Return from main loop.
HandleBestScore:
   ld a,(NewBestScoreFlag)
   cp FLAG_UP
   jp nz,+
   ld hl,Score                ; If today's best score is beaten, copy the current
   ld de,TodaysBestScore      ; score to today's best score every frame, else...
   ldi
   ldi
   ret 
+:                            ; If it is not already beaten, see if it happens now!
   ld a,(TodaysBestScore)
   ld b,a
   ld a,(Score)
   sub b
   ret c
   cp 1
   jp z,+
   ld a,(TodaysBestScore+1)   ; First digit is equal or higher...
   ld b,a
   ld a,(Score+1)
   cp b
   ret z
   ret c
+:
   ld a,FLAG_UP               ; Second digit is higher = best score is beaten!
   ld (NewBestScoreFlag),a
   ld hl,NewBestScoreSFX      ; Play the little jingle.
   call PSGPlayNoRepeat
   ret
HandleGameModeCounter:        ; Add 1 to a counter every frame. As it reaches 255,
   ld a,(GameMode)            ; increment another byte-sized counter. Compare the
   cp HARD_MODE               ; second counter against the HARD_MODE_THRESHOLD.
   ret z
   ld a,(GameModeCounter)
   inc a
   ld (GameModeCounter),a
   cp 255
   ret nz
   ld a,(GameModeCounter+1)
   inc a
   ld (GameModeCounter+1),a
   cp HARD_MODE_THRESHOLD
   ret nz
   ld a,HARD_MODE
   ld (GameMode),a
   ret
ScrollRacetrack:              ; Load new vertical scroll value into the buffer.
   ld a,(Scroll)
   sub PLAYER_VERTICAL_SPEED
   ld (Scroll),a
   ret
AnimateCar:                   ; Used by both player and enemy cars.
   push hl                    ; Save pointer to meta sprite data.
   ld a,(ix+4)                ; Get current cel.
   inc a
   cp MAX_CELS+1
   jp nz,+
   xor a                      ; Overflow - back to first cel.
+:
   ld (ix+4),a                ; Save the new cel number to ram.
   add a,a                    ; Get the word-sized table element (the address).
   ld l,a
   ld h,0
   add hl,bc                  ; BC holds the cel table.
   ld e,(hl)
   inc hl
   ld d,(hl)
   pop hl                     ; Retrieve pointer to meta sprite data.
   ld (hl),e
   inc hl
   ld (hl),d
   ret
UpdateNameTableBuffers:
   call UpdateScoreBuffer
   call UpdateTodaysBestScoreBuffer
   ret
UpdateScoreBuffer:
   ld a,(Score)
   add a,a
   add a,SCORE_TILE_OFFSET
   ld ix,ScoreBuffer
   ld (ix+0),a
   inc a
   ld (ix+4),a
   ld a,(Score+1)
   add a,a
   add a,SCORE_TILE_OFFSET
   ld ix,ScoreBuffer+2
   ld (ix+0),a
   inc a
   ld (ix+4),a
   ret
UpdateTodaysBestScoreBuffer:
   ld a,(TodaysBestScore)
   add a,a
   add a,SCORE_TILE_OFFSET
   ld ix,TodaysBestScoreBuffer
   ld (ix+0),a
   inc a
   ld (ix+4),a
   ld a,(TodaysBestScore+1)
   add a,a
   add a,SCORE_TILE_OFFSET
   ld ix,TodaysBestScoreBuffer+2
   ld (ix+0),a
   inc a
   ld (ix+4),a
   ret
 UpdateSATBuffers:
   ld ix,PlayerY
   call UpdateCar
   ld ix,Ash
   call UpdateCar
   ld ix,May
   call UpdateCar
   ld ix,Iris
   call UpdateCar
   ret
UpdateCar:
   call HandleY                  ; Calc. y-positions and load them to SpriteBufferY.
   call HandleXC                 ; Calc. and load x-positions and character codes.
   ret
HandleY:
   ld hl,SpriteBufferY
   ld a,(ix+5)                   ; Get the car's index.
   add a,a                       ; Start writing at buffer offset 8 x index.
   add a,a
   add a,a
   ld d,0
   ld e,a
   add hl,de
   ex de,hl                      ; DE points to first y-pos in buffer.
   ld b,8
   ld a,(ix+0)                   ; Get the car's y-position.
   ld c,a                        ; Save it in register C.
   ld h,(ix+3)                   ; Get the meta sprite data pointer,
   ld l,(ix+2)                   ; and store it in HL.
-:
   ld a,(hl)                     ; Read offset.
   add a,c                       ; Apply saved y-position to offset.
   ld (de),a
   inc de
   inc hl
   djnz -                        ; Perform all 8 loops, then continue...
   ret
HandleXC:
   ld h,(ix+3)                   ; Get the meta sprite data pointer,
   ld l,(ix+2)                   ; and store it in HL.
   ld d,0                        ; Now skip the first 8 bytes (the y-positions).
   ld e, 8
   add hl,de
   push hl                       ; Save the offset source pointer on the stack.
   ld hl,SpriteBufferXC          ; Set up a destination pointer in the buffer.
   ld a,(ix+5)
   add a,a                       ; Offset = 16 x index.
   add a,a
   add a,a
   add a,a
   ld d,0
   ld e,a
   add hl,de
   ex de,hl                      ; DE points to where we willwrite the first XC pair.
   ld b,8
   ld a,(ix+1)                   ; Get the car's x-position.
   ld c,a                        ; Save it in register C.
   pop hl                        ; Retrieve the meta sprite source pointer from stack.
-:
   ld a,(hl)                     ; Read offset.
   add a,c                       ; Apply saved x-position to offset.
   ld (de),a                     ; Save this to the buffer.
   inc de
   inc hl
   ld a,(hl)                     ; Get the character code.
   ld (de),a                     ; Save this in the buffer
   inc de
   inc hl
   djnz -                        ; Perform all 8 loops, then continue...
   ret
Death:     
   ld a,(GameBeatenFlag)
   cp FLAG_UP
   jp z,+                        ; Don't play crash sound if player beats the game!
   call PSGSFXStop
   ld hl,Crash
   call PSGPlayNoRepeat
+:
   ld b,DEATH_DELAY
-:
   push bc
   call PSGFrame
   pop bc
   halt
   djnz -
   ld hl,Sprites_Palette
   ld b,SPRITE_PALETTE_SIZE
   ld a,START_OF_SPRITE_PALETTE
   call FadeOutScreen
   ret
.ends
; ---------------------
.section "The player" free
InitializePlayer:
   ld hl,FIRST_PLAYER_TILE
   PrepareVram
   ld hl,PlayerCar_Tiles
   ld bc,PLAYER_METASPRITE_SIZE
   call LoadVRAM
   ld a,PLAYER_Y_START
   ld (PlayerY),a
   ld a,PLAYER_X_START
   ld (PlayerX),a
   ld hl,PlayerCel0
   ld (PlayerMetaSpriteDataPointer),hl
   ld ix,PlayerY
   call UpdateCar
   ret
AnimatePlayer:
   ld ix,PlayerY
   ld bc, PlayerCelTable
   ld hl,PlayerMetaSpriteDataPointer
   call AnimateCar
   ret
MovePlayer:
   ld a,(Joystick1)
   bit PLAYER1_JOYSTICK_RIGHT,a
   jp nz,+
   ld hl,RandomSeed+1               ; Modify MSB of seed whenever player moves right.
   dec (hl)
   ld a,(PlayerX)
   cp RIGHT_BORDER
   jp nc,+
   add a,PLAYER_HORIZONTAL_SPEED
   ld (PlayerX),a
   ret
+:
   bit PLAYER1_JOYSTICK_LEFT,a
   ret nz
   ld hl,RandomSeed+1               ; Modify MSB of seed whenever player moves left.
   inc (hl)
   ld a,(PlayerX)
   cp LEFT_BORDER
   ret c
   sub PLAYER_HORIZONTAL_SPEED
   ld (PlayerX),a
   ret
.ends
; ---------------------
.section "Enemy code" free
InitializeEnemies:
   ld hl,FIRST_ENEMY_TILE
   PrepareVram
   ld hl,EnemyCar_Tiles
   ld bc,ENEMY_METASPRITE_SIZE
   call LoadVRAM
   ld a,ASH_Y_START
   ld (Ash.y),a
   ld a,ASH_X_START
   ld (Ash.x),a
   ld hl,DisabledCar
   ld (Ash.metasprite),hl
   ld a,1
   ld (Ash.index),a
   ld a,MAY_Y_START
   ld (May.y),a
   ld a,MAY_X_START
   ld (May.x),a
   ld hl,DisabledCar
   ld (May.metasprite),hl
   ld a,2
   ld (May.index),a
   ld a,IRIS_Y_START
   ld (Iris.y),a
   ld a,IRIS_X_START
   ld (Iris.x),a
   ld hl,DisabledCar
   ld (Iris.metasprite),hl
   ld a,3
   ld (Iris.index),a
   xor a
   ld (Ash.status),a
   ld (May.status),a
   ld (Iris.status),a
   ret
MoveEnemies:
   ld ix,Ash
   call MoveEnemy
   ld ix,May
   call MoveEnemy
   ld ix,Iris
   call MoveEnemy
   ret
 MoveEnemy:
   call MoveEnemyVertically
   call MoveEnemyHorizontally
   ret
MoveEnemyVertically:
   ld a,(ix+0)                ; Get enemy y-position.
   add a,ENEMY_VERTICAL_SPEED
   ld (ix+0),a
   cp BOTTOM_BORDER
   call z,ResetEnemy
   ld a,(ix+7)
   cp ENABLED
   ret nz
   ld a,(ix+0)
   cp SCORE_LINE
   call z,IncrementScore
   ret
IncrementScore:
   ld a,(Score+1)
   cp 9
   jp z,+
   inc a
   ld (Score+1),a
   ret
+:
   xor a
   ld (Score+1),a
   ld a,(Score)
   cp 9
   jp nz,+
   ld a,FLAG_UP
   ld (GameBeatenFlag),a
   ld hl,GAME_BEATEN_SCORE    ; End on a score of 99.
   ld (Score),hl
   ret
+:
   inc a
   ld (Score),a
   ret
MoveEnemyHorizontally:
   ld a,(ix+6)                ; Get direction
   cp GOING_RIGHT
   jp nz,+
   call MoveEnemyRight
   ret
+:
   cp GOING_LEFT
   call z,MoveEnemyLeft
   ret
MoveEnemyRight:
   ld a,(ix+1)                ; Get enemy x-position.
   cp ENEMY_RIGHT_BORDER
   jp c,+
   call ToggleEnemyDirection
   ret
+:
   add a,ENEMY_HORIZONTAL_SPEED
   ld (ix+1),a
   ret
MoveEnemyLeft:
   ld a,(ix+1)                ; Get enemy x-position.
   cp ENEMY_LEFT_BORDER
   jp nc,+
   call ToggleEnemyDirection
   ret
+:
   sub ENEMY_HORIZONTAL_SPEED
   ld (ix+1),a
   ret
ToggleEnemyDirection:
   ld a,(ix+6)                ; Get enemy direction.
   cp GOING_LEFT
   jp nz,+
   ld a,GOING_RIGHT
   ld (ix+6),a
   ret
+:
   ld a,GOING_LEFT
   ld (ix+6),a
   ret
ResetEnemy:
   call GetRandomNumber
   and %00000111              ; Shoudl this car be disabled?
   cp 0
   jp nz,+
   ld a,DISABLED              ; Yes....
   ld (ix+7),a
   ld hl,DisabledCar
   ld (ix+2),l
   ld (ix+3),h
   ret
+:
   ld a,ENABLED
   ld (ix+7),a
   ld hl,GreenCarCel0
   ld (ix+2),l
   ld (ix+3),h
   call GetRandomNumber
   and %00001111              ; Apply mask so that A contains a value between 0-31.
   ld d,0
   ld e,a
   ld hl,RespawnTable
   add hl,de
   ld a,(hl)                  ; Get a starting x-pos. from the table.
   ld (ix+1),a                ; Enemy's x-position.
   ld a,(GameMode)
   cp EASY_MODE
   jp nz,+
   call GetRandomNumber
   and EASY_MODE_MASK         ; Will the car move r/l, or just straight down?
   ld (ix+6),a
   ret
+:
   call GetRandomNumber
   and HARD_MODE_MASK         ; Will the car move r/l, or just straight down?
   ld (ix+6),a
   ret
AnimateEnemies:
   ld a,(Ash.status)          ; If an enemy car is enabled, it will be animated...
   cp 0
   jp z,+
   ld ix,Ash
   ld bc, EnemyCelTable
   ld hl,Ash.metasprite 
   call AnimateCar
+:
   ld a,(May.status)
   cp 0
   jp z,+
   ld ix,May
   ld bc,EnemyCelTable
   ld hl,May.metasprite
   call AnimateCar
+:
   ld a,(Iris.status)
   cp 0
   jp z,+
   ld ix,Iris
   ld bc,EnemyCelTable
   ld  hl,Iris.metasprite
   call AnimateCar
+:
   ret
.ends
; ---------------------
.section "Celebrate" free
Celebrate:
   di
   ld a,TURN_SCREEN_OFF
   ld b,VDP_REGISTER_1
   call SetRegister
   ld hl,SAT_Y_TABLE
   PrepareVram
   ld c,VDP_DATA
   ld a,SPRITE_TERMINATOR
   out (c),a                  ; Kill the sprites.
   ld a,0
   ld b,VDP_VERTICAL_SCROLL_REGISTER
   call SetRegister
   ld ix,CelebrationscreenImageData
   call LoadImage
   ld a,TURN_SCREEN_ON_TALL_SPRITES
   ld b,VDP_REGISTER_1
   call SetRegister
   call PSGSFXStop
   call PSGStop
   ld hl,CelebrateSound
   call PSGPlayNoRepeat
   ei
CelebrationLoop:
   call WaitForFrameInterrupt
   call PSGFrame
   call Housekeeping
   ld a,(Joystick1)
   bit PLAYER1_START,a
   ret z
   jp CelebrationLoop
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
   .include "Race/rt_tiles.inc"
   
RacetrackTilesEnd:
RacetrackTilemap:
   .include "Race/rt_tilemap.inc"
RacetrackPalette:
   .include "Race/rt_palette.inc"
RacetrackPaletteEnd:
PlayerCar_Tiles:
   .include "Race/PlayerCar_tiles.inc"
ScoreDigits_Tiles:
   .include "Race/ScoreDigits_tiles.inc"
EnemyCar_Tiles:
   .include "Race/EnemyCar_tiles.inc"
Sprites_Palette:
   .include "Race/Sprites_palette.inc"
RacetrackMockupData:
   .dw RacetrackTiles    ; Pointer to tile data.
   .dw RacetrackTilesEnd-RacetrackTiles ; Tile data (bytes) to write.
   .dw RacetrackTilemap ; Pointer to tilemap data.
   .dw WHOLE_NAMETABLE  ; Overwrite the whole nametable.
   .dw RacetrackPalette ; Pointer to palette.
   .dw RacetrackPaletteEnd-RacetrackPalette ; Amount of colors.

TitlescreenTiles:
   .include "Title/Titlescreen_tiles.inc"
TitlescreenTilesEnd:
TitlescreenTilemap:
   .include "Title/Titlescreen_tilemap.inc"
TitlescreenPalette:
   .include "Title/Titlescreen_palette.inc"
TitlescreenPaletteEnd:
TitlescreenImageData:
   .dw TitlescreenTiles  ; Pointer to tile data.
   .dw TitlescreenTilesEnd-TitlescreenTiles ; Tile data (bytes) to write.
   .dw TitlescreenTilemap ; Pointer to tilemap data.
   .dw VISIBLE_PART_OF_SCREEN ; Amount of bytes to write to tilemap.
   .dw TitlescreenPalette ; Pointer to palette.
   .dw TitlescreenPaletteEnd-TitlescreenPalette ; Amount of colors.

CelebrationscreenTiles:
   .include "Celebrate/celebrate_tiles.inc"
CelebrationscreenTilesEnd:
CelebrationscreenTilemap:
   .include "Celebrate/celebrate_tilemap.inc"
CelebrationscreenPalette:
   .include "Celebrate/celebrate_palette.inc"
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