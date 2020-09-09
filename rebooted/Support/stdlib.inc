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