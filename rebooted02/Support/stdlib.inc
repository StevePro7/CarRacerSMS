; Standard SMS library.

; --------------------------------------------------------------
.section "FadeOutScreen" free
; --------------------------------------------------------------
; Fade out screen. (c) Kagesan, modified.
; ENTRY: HL = pointer to palette, B = amount of colors, A = First palette item.
FadeOutScreen:
    push bc
    push hl
    ;xor a
    out ($bf),a             ; palette index (0)
    ld a,$c0
    out ($bf),a             ; palette write identifier

    ;ld b,32                ; number of palette entries: 32 (full palette)
    ;ld hl,Sprites_Palette  ; source
-:  ld a,(hl)               ; load raw palette data
    and %00101010           ; modify color values: 3 becomes 2, 1 becomes 0
    out ($be),a             ; write modified data to CRAM
    inc hl
    djnz -

    ld b,6                  ; delay 6 frames
-:  halt
    djnz -
    
    pop hl
    pop bc
    push bc
    ;ld b,32                ; number of palette entries: 32 (full palette)
    ;ld hl,Sprites_Palette  ; source
-:  ld a,(hl)               ; load raw palette data
    and %00101010           ; modify color values: 3 becomes 2, 1 becomes 0
    srl a                   ; modify color values: 2 becomes 1
    out ($be),a             ; write modified data to CRAM
    inc hl
    djnz -

    ld b,6                  ; delay 6 frames
-:  halt
    djnz -
    
    pop bc
;   ld b, 32                ; number of palette entries: 32 (full palette)
    xor a                   ; we want to blacken the palette, so a is set to 0
-:  out ($be), a            ; write zeros to CRAM, palette fade complete
    djnz -
    ret
.ends

; --------------------------------------------------------------
.section "Random number" free
; --------------------------------------------------------------
GetRandomNumber:    ; (c) SMS-Power.
; Uses a 16-bit RAM variable called RandomNumberGeneratorWord
; Returns an 8-bit pseudo-random number in a
    push hl
        ld hl,(RandomSeed)
        ld a, h         ; get high byte
        rrca            ; rotate right by 2
        rrca
        xor h           ; xor with original
        rrca            ; rotate right by 1
        xor l           ; xor with low byte
        rrca            ; rotate right by 4
        rrca
        rrca
        rrca
        xor l           ; xor again
        rra             ; rotate right by 1 through carry
        adc hl, hl      ; add RandomNumberGeneratorWord to itself
        jr nz,+
        ld hl,$733c     ; if last xor resulted in zero then re-seed random number generator
+:      ld a,r          ; r = refresh register = semi-random number
        xor l           ; xor with l which is fairly random
        ld (RandomSeed),hl
    pop hl
    ret                 ; return random number in a
.ends

; --------------------------------------------------------------
.section "Prepare VRAM" free
; --------------------------------------------------------------
; PREPARE VRAM.
; Set up vdp to receive data at vram address in HL.
; Obsolete! Usually achieved with rst $20.
PrepareVRAM:
    push af
        ld a,l
        out ($bf),a
        ld a,h
        or $40
        out ($bf),a
    pop af
    ret
.ends

; --------------------------------------------------------------
.section "Load VRAM" free
; --------------------------------------------------------------
; LOAD VRAM.
; Load BC amount of bytes from data source pointed to by HL.
; Tip: Prepare VRAM before calling.
LoadVRAM:
    ld a,(hl)
    out ($be),a
    inc hl
    dec bc
    ld a,c
    or b
    jp nz,LoadVRAM
    ret
.ends

; --------------------------------------------------------------
.section "Fill memory" free
; --------------------------------------------------------------
; FILL MEMORY.
; Overwrite a chunk of memory with a specified byte value.
; HL = base address, BC = area size, A = fill byte.
; Used by ClearRam.
FillMemory:
    ld (hl),a
    ld d,h
    ld e,l
    inc de
    dec bc
    ld a,b
    or c
    ret z
    ldir
    ret
.ends

; --------------------------------------------------------------
.section "Set VDP Register" free
; --------------------------------------------------------------
; SET VDP REGISTER.
; Write to target register.
; A = byte to be loaded into vdp register.
; B = target register 0-10.
SetRegister:
    out ($bf),a
    ld a,$80
    or b
    out ($bf),a
    ret
.ends

; --------------------------------------------------------------
.section "Get Vector" free
; --------------------------------------------------------------
; GET VECTOR.
; Vector table based switch-like control structure.
; A = expression, DE = vector table (word-sized vectors).
; Return vector in HL for a jp (hl) opcode.
GetVector:
    sla a
    ld h,0
    ld l,a
    add hl,de
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a
    ret
.ends

; --------------------------------------------------------------
.section "Wait for frame interrupt" free
; --------------------------------------------------------------
; WAIT FOR FRAME INTERRUPT.
; This will wait exclusively for the frame interrupt.
; Unlike the 'halt' opcode. Assumes the variable VDPStatus is set by
; the interrupt handler. Clears bit 7 of VDPStatus upon return.
WaitForFrameInterrupt:
    ld a,(VDPStatus)
    bit 7,a
    jp z,WaitForFrameInterrupt
    res 7,a
    ld (VDPStatus),a
    ret
.ends

; --------------------------------------------------------------
.section "Read Joysticks" free
; --------------------------------------------------------------
; READ JOYSTICKS.
; Read the two joystick ports (port $dc and $dd) into ram.

ReadJoysticks:
    in a,$dc
    ld (Joystick1),a
    in a,$dd
    ld (Joystick2),a
    ret
.ends

; --------------------------------------------------------------
.section "Get Word" free
; --------------------------------------------------------------
; GET WORD.
; Return an element from a word-sized table.
; Entry: A = Index, HL = Pointer to table.
; Exit: HL contains the element in the byte order it was stored.
GetWord:
    sla a           ; Multiply by 2 because the elements are words.
    ld d,0
    ld e,a
    add hl,de       ; Now HL points to the element.
    ld a,(hl)
    ld e,a
    inc hl
    ld a,(hl)
    ld d,a
    ex de,hl
    ret
.ends

; -----------------------------------------------------------------------------
.section "Clear Ram" free
; -----------------------------------------------------------------------------
; Write zeroes to ram, and then use the zeroed ram to clear vram.
ClearRam:
    ; Overwrite 4K of RAM with zeroes.
    ld hl,$c000
    ld bc,$1000
    ld a,0
    call FillMemory
    
    ; Use the initialized ram to clear all of vram.
    ld hl,$0000
    rst $20
    ld b,4
-   push bc
    ld hl,$c000
    ld bc,$1000
    call LoadVRAM
    pop bc
    djnz -
    ret
.ends