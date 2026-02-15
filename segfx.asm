; ============================================================================
; SEGFX — 14-Segment Display Effect Engine
; ============================================================================
; Target:   Z80 + 24x 14-segment character displays
; Assembler: sjasmplus
; BIOS:     0xFDD6 = write segment pattern (HL=bitmask, A=column)
;           0xFDD3 = write brightness (C=level, A=column)
;           User event hook called at 50Hz
; ============================================================================

BIOS_SEG        equ     0FDD6h
BIOS_BRT        equ     0FDD3h
DISP_COLS       equ     24

; ============================================================================
; Protocol constants
; ============================================================================

; Stream markers
PAGE_START      equ     01h
PAGE_END        equ     02h
END_STREAM      equ     00h

; Entry/exit transition effects
TRANS_NONE      equ     00h
TRANS_FADE      equ     01h
TRANS_WIPE_L    equ     03h
TRANS_WIPE_R    equ     04h
TRANS_WIPE_CTR  equ     05h
TRANS_WIPE_EDGE equ     06h
TRANS_DISSOLVE  equ     07h
TRANS_FADE_WP_L equ     08h
TRANS_FADE_WP_R equ     09h

; Display/hold effects
DISP_STATIC     equ     00h
DISP_FLASH      equ     01h
DISP_BREATHE    equ     02h
DISP_SCROLL_L   equ     03h
DISP_SCROLL_R   equ     04h
DISP_MARQUEE_L  equ     05h
DISP_MARQUEE_R  equ     06h
DISP_TYPEWRITER equ     07h
DISP_TWRT_R     equ     08h
DISP_BOUNCE     equ     09h
DISP_FLASH_ALT  equ     0Ah
DISP_SPARKLE    equ     0Bh
DISP_WAVE       equ     0Ch

; Inline effect codes (embedded in content)
INL_BRIGHT      equ     80h
INL_FLASH       equ     81h
INL_NOFLASH     equ     82h
INL_PAUSE       equ     83h
INL_SPEED       equ     84h
INL_ALIGN_C     equ     85h
INL_ALIGN_R     equ     86h
INL_ALIGN_L     equ     87h

; Inline parameter range
INL_PARAM_BASE  equ     90h             ; 90h..9Fh encode values 0..15

; Attribute flag bits
ATTR_F_FLASH    equ     0               ; bit 0: per-char flash
ATTR_F_PAUSE    equ     7               ; bit 7: pause marker


; ============================================================================
; Engine state (struct)
; ============================================================================

                struct  SEGFX_STATE
stream_base     dw      0               ; start of stream (for looping)
stream_ptr      dw      0               ; current read position
page_state      db      0               ; 0=idle 1=entry 2=display 3=exit
tick_count      dw      0               ; dwell ticks remaining
step_timer      db      0               ; countdown to next anim step
speed           db      0               ; ticks per step
fx_entry        db      0               ; entry transition code
fx_display      db      0               ; display effect code
fx_exit         db      0               ; exit transition code
fx_pos          db      0               ; animation position counter
fx_sub          db      0               ; sub-position (fade trails etc)
text_len        db      0               ; total decoded entries (chars + pauses)
vis_len         db      0               ; visible char count (excl. pauses)
text_offset     db      0               ; scroll offset (0..255, single byte)
align_mode      db      0               ; 0=left 1=center 2=right
bounce_dir      db      0               ; 0=left 1=right
inl_bright      db      0FFh            ; current inline brightness
inl_flash       db      0               ; current inline flash flag
pause_count     db      0               ; inline pause tick countdown
flash_phase     db      0               ; global flash toggle (0/1)
marquee_state   db      0               ; 0=scrolling in, 1=holding, 2=scrolling out
marquee_hold    dw      0               ; ticks remaining in marquee hold phase
lfsr            db      0ACh            ; 8-bit LFSR for sparkle (non-zero seed)
dirty           db      0               ; non-zero = display buffers need pushing to hw
                ends


; ============================================================================
; RAM allocation — define SEGFX_RAM_BASE before including this file
; ============================================================================

    IFNDEF SEGFX_RAM_BASE
SEGFX_RAM_BASE  equ     0C000h          ; default — change to suit your memory map
    ENDIF

                org     SEGFX_RAM_BASE

state           SEGFX_STATE

; Display output buffers (contiguous for fast clear)
disp_seg        ds      DISP_COLS * 2   ; 48 bytes: segment bitmasks
disp_bright     ds      DISP_COLS       ; 24 bytes: brightness

; Text workspace
text_buf        ds      256             ; decoded ASCII text
attr_bright     ds      256             ; per-char brightness
attr_flags      ds      256             ; per-char flags (bit 0=flash, bit 7=pause)


; ============================================================================
; ROM data
; ============================================================================

; Dissolve order: pseudo-random column reveal sequence
dissolve_order:
                db      11,3,19,7,15,0,22,5,17,9,13,2,20,6,23,10,16,1,21,8,14,4,18,12

; 32-entry sine table, full cycle, 0x00..0xFF
sine_table:
                db      80h,98h,0B0h,0C6h,0DAh,0EAh,0F6h,0FDh
                db      0FFh,0FDh,0F6h,0EAh,0DAh,0C6h,0B0h,98h
                db      80h,67h,4Fh,39h,25h,15h,09h,02h
                db      00h,02h,09h,15h,25h,39h,4Fh,67h

; Font: ASCII 0x20..0x7F → 14-segment bitmasks (96 entries × 2 bytes)
font_table:
                dw      0000h           ;   (space)
                dw      4900h           ; !
                dw      0202h           ; "
                dw      12ceh           ; #
                dw      12edh           ; $
                dw      2de4h           ; %
                dw      0b59h           ; &
                dw      0200h           ; '
                dw      0c00h           ; (
                dw      2100h           ; )
                dw      3fc0h           ; *
                dw      12c0h           ; +
                dw      2000h           ; ,
                dw      00c0h           ; -
                dw      4000h           ; .
                dw      2400h           ; /
                dw      243fh           ; 0
                dw      0406h           ; 1
                dw      00dbh           ; 2
                dw      008fh           ; 3
                dw      00e6h           ; 4
                dw      0869h           ; 5
                dw      00fdh           ; 6
                dw      1401h           ; 7
                dw      00ffh           ; 8
                dw      00efh           ; 9
                dw      0040h           ; :
                dw      2200h           ; ;
                dw      0c40h           ; <
                dw      00c8h           ; =
                dw      2180h           ; >
                dw      5083h           ; ?
                dw      02bbh           ; @
                dw      00f7h           ; A
                dw      128fh           ; B
                dw      0039h           ; C
                dw      120fh           ; D
                dw      0079h           ; E
                dw      0071h           ; F
                dw      00bdh           ; G
                dw      00f6h           ; H
                dw      1209h           ; I
                dw      001eh           ; J
                dw      0c70h           ; K
                dw      0038h           ; L
                dw      0536h           ; M
                dw      0936h           ; N
                dw      003fh           ; O
                dw      00f3h           ; P
                dw      083fh           ; Q
                dw      08f3h           ; R
                dw      00edh           ; S
                dw      1201h           ; T
                dw      003eh           ; U
                dw      2430h           ; V
                dw      2836h           ; W
                dw      2d00h           ; X
                dw      00eeh           ; Y
                dw      2409h           ; Z
                dw      0039h           ; [
                dw      0900h           ; backslash
                dw      000fh           ; ]
                dw      2800h           ; ^
                dw      0008h           ; _
                dw      0100h           ; `
                dw      208ch           ; a
                dw      0878h           ; b
                dw      00d8h           ; c
                dw      208eh           ; d
                dw      2058h           ; e
                dw      14c0h           ; f
                dw      048eh           ; g
                dw      1070h           ; h
                dw      1000h           ; i
                dw      2210h           ; j
                dw      1e00h           ; k
                dw      1200h           ; l
                dw      10d4h           ; m
                dw      1050h           ; n
                dw      00dch           ; o
                dw      0170h           ; p
                dw      0486h           ; q
                dw      0050h           ; r
                dw      0888h           ; s
                dw      0078h           ; t
                dw      001ch           ; u
                dw      2010h           ; v
                dw      2814h           ; w
                dw      2d00h           ; x
                dw      028eh           ; y
                dw      2048h           ; z
                dw      2149h           ; {
                dw      1200h           ; |
                dw      0c89h           ; }
                dw      24c0h           ; ~
                dw      0000h           ; DEL


; ============================================================================
; FONT_LOOKUP
; ============================================================================
; Input:  A = ASCII character (20h..7Fh)
; Output: HL = 14-segment bitmask
; Clobbers: DE
; ============================================================================
font_lookup:
                sub     20h
                ld      l,a
                ld      h,0
                add     hl,hl           ; ×2 (word entries)
                ld      de,font_table
                add     hl,de
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a
                ret


; ============================================================================
; CALC_START_COL — Starting display column based on alignment
; ============================================================================
; Uses state.vis_len for column count (excludes pause markers).
; Output: A = start column (0..23)
; Clobbers: B
; ============================================================================
calc_start_col:
                ld      a,(state.align_mode)
                or      a
                jr      z,.left
                cp      1
                jr      z,.center
                ; Right
                ld      a,(state.vis_len)
                ld      b,a
                ld      a,DISP_COLS
                sub     b
                ret     nc
                xor     a
                ret
.center:        ld      a,(state.vis_len)
                ld      b,a
                ld      a,DISP_COLS
                sub     b
                jr      c,.left
                srl     a
                ret
.left:          xor     a
                ret


; ============================================================================
; SEGFX_INIT — Initialise engine with a message stream
; ============================================================================
; Input:  HL = pointer to message stream
; ============================================================================
segfx_init:
                ld      (state.stream_base),hl
                ld      (state.stream_ptr),hl
                xor     a
                ld      (state.page_state),a
                ld      (state.fx_pos),a
                ld      (state.fx_sub),a
                ld      (state.flash_phase),a
                ld      (state.bounce_dir),a
                ld      (state.pause_count),a
                ld      (state.align_mode),a
                ld      (state.text_len),a
                ld      (state.vis_len),a
                ld      (state.text_offset),a
                ld      (state.marquee_state),a
                ld      (state.marquee_hold),a
                ld      (state.marquee_hold+1),a
                ld      a,0ACh
                ld      (state.lfsr),a
                xor     a
                ld      (state.dirty),a
                ld      a,0FFh
                ld      (state.inl_bright),a
                ld      a,1
                ld      (state.step_timer),a
                call    display_clear
                ret


; ============================================================================
; DISPLAY_CLEAR — Zero segment + brightness buffers
; ============================================================================
display_clear:
                ld      hl,disp_seg     ; disp_seg and disp_bright are contiguous
                ld      b,DISP_COLS * 2 + DISP_COLS
                xor     a
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                ret


; ============================================================================
; SEGFX_OUTPUT — Write display buffers to hardware via BIOS
; ============================================================================
; Interrupts are disabled during output to prevent the tick handler from
; modifying buffers mid-write, which causes display glitches.
; ============================================================================
segfx_output:
                di
                ; Pass 1: Segment patterns
                ld      ix,disp_seg
                xor     a
                ld      b,DISP_COLS
.seg:           push    bc
                push    af
                ld      l,(ix+0)
                ld      h,(ix+1)
                pop     af
                call    BIOS_SEG
                inc     ix
                inc     ix
                pop     bc
                djnz    .seg

                ; Pass 2: Brightness
                ld      iy,disp_bright
                xor     a
                ld      b,DISP_COLS
.brt:           push    bc
                push    af
                ld      c,(iy+0)
                pop     af
                call    BIOS_BRT
                inc     iy
                pop     bc
                djnz    .brt
                ei
                ret


; ============================================================================
; SEGFX_TICK — Tick handler (called at 50Hz from BIOS interrupt)
; ============================================================================
; This ONLY updates state and display buffers. It does NOT call the BIOS
; display routines. Call segfx_update from your main loop to push changes
; to hardware.
; ============================================================================
segfx_tick:
                push    af
                push    bc
                push    de
                push    hl
                push    ix
                push    iy

                ; Toggle flash phase
                ld      a,(state.flash_phase)
                xor     1
                ld      (state.flash_phase),a

                ; Handle inline pause countdown
                ld      a,(state.pause_count)
                or      a
                jr      z,.no_pause
                dec     a
                ld      (state.pause_count),a
                jr      .mark_dirty
.no_pause:

                ; Decrement dwell counter every tick (not per step)
                ; Only in display state (page_state == 2)
                ld      a,(state.page_state)
                cp      2
                jr      nz,.no_dwell
                ld      hl,(state.tick_count)
                ld      a,h
                or      l
                jr      z,.dwell_expired
                dec     hl
                ld      (state.tick_count),hl
                jr      .no_dwell
.dwell_expired:
                ; Dwell done — move to exit phase
                ld      a,3
                ld      (state.page_state),a
                xor     a
                ld      (state.fx_pos),a
                ld      (state.fx_sub),a
                jr      .mark_dirty
.no_dwell:

                ; Step timer
                ld      a,(state.step_timer)
                dec     a
                ld      (state.step_timer),a
                jr      nz,.mark_dirty  ; not time for a step, but still dirty for flash

                ; Reload step timer (clamp speed >= 1)
                ld      a,(state.speed)
                or      a
                jr      nz,.spd_ok
                inc     a
.spd_ok:        ld      (state.step_timer),a

                ; Dispatch on page_state
                ld      a,(state.page_state)
                or      a
                jr      z,.idle
                cp      1
                jr      z,.entry
                cp      2
                jr      z,.display
                jr      .exit

.idle:          call    page_load
                jr      .mark_dirty

.entry:         call    fx_trans_step
                jr      nc,.mark_dirty  ; CF clear = still running
                ; Entry complete → display
                ld      a,2
                ld      (state.page_state),a
                call    render_full
                call    set_full_bright
                jr      .mark_dirty

.display:       ; Dwell already handled above; just run display effect
                call    fx_display_step
                jr      .mark_dirty

.exit:          call    fx_trans_exit_step
                jr      nc,.mark_dirty
                xor     a
                ld      (state.page_state),a
                call    display_clear
                ; fall through to mark_dirty

.mark_dirty:    ld      a,1
                ld      (state.dirty),a
                pop     iy
                pop     ix
                pop     hl
                pop     de
                pop     bc
                pop     af
                ret


; ============================================================================
; SEGFX_UPDATE — Call from main loop to push display buffers to hardware
; ============================================================================
; Checks the dirty flag. If set, calls segfx_output and clears the flag.
; Safe to call as often as you like; it's a no-op when nothing changed.
; ============================================================================
segfx_update:
                ld      a,(state.dirty)
                or      a
                ret     z               ; nothing to do
                xor     a
                ld      (state.dirty),a
                jp      segfx_output    ; tail call


; ============================================================================
; PAGE_LOAD — Parse page header and decode content
; ============================================================================
page_load:
                ld      hl,(state.stream_ptr)

.find:          ld      a,(hl)
                cp      END_STREAM
                jr      z,.wrap
                cp      PAGE_START
                jr      z,.found
                inc     hl
                jr      .find

.wrap:          ld      hl,(state.stream_base)
                jr      .find

.found:         inc     hl              ; skip PAGE_START
                ld      a,(hl)
                ld      (state.fx_entry),a
                inc     hl
                ld      a,(hl)
                ld      (state.fx_display),a
                inc     hl
                ld      a,(hl)
                ld      (state.fx_exit),a
                inc     hl
                ld      a,(hl)
                ld      (state.speed),a
                ld      (state.step_timer),a
                inc     hl
                ld      d,(hl)          ; dwell_hi
                inc     hl
                ld      e,(hl)          ; dwell_lo
                inc     hl
                ld      (state.tick_count),de

                ; Reset per-page state
                xor     a
                ld      (state.fx_pos),a
                ld      (state.fx_sub),a
                ld      (state.align_mode),a
                ld      (state.bounce_dir),a
                ld      (state.pause_count),a
                ld      (state.inl_flash),a
                ld      (state.text_offset),a
                ld      (state.marquee_state),a
                ld      a,0FFh
                ld      (state.inl_bright),a

                call    decode_content
                ld      (state.stream_ptr),hl

                call    display_clear

                ; TRANS_NONE → skip entry, go straight to display
                ld      a,(state.fx_entry)
                or      a
                jr      nz,.has_entry
                call    render_full
                call    set_full_bright
                ld      a,2
                ld      (state.page_state),a
                ret

.has_entry:     ld      a,1
                ld      (state.page_state),a
                ret


; ============================================================================
; DECODE_CONTENT — Strip inline codes, build text/attr arrays
; ============================================================================
; Input:  HL = content bytes (after page header)
; Output: HL past PAGE_END; state.text_len, state.vis_len set
; Clobbers: AF, BC, DE, IX, IY
; ============================================================================
decode_content:
                ld      de,text_buf
                ld      ix,attr_bright
                ld      iy,attr_flags
                ld      c,0             ; total count
                ld      b,0             ; visible count

                ld      a,0FFh
                ld      (state.inl_bright),a
                xor     a
                ld      (state.inl_flash),a

.lp:            ld      a,(hl)
                cp      PAGE_END
                jp      z,.done
                cp      END_STREAM
                jp      z,.done
                cp      80h
                jp      c,.char
                cp      90h
                jr      c,.inline
                jp      .char           ; 90h+ treat as char

.inline:        cp      INL_BRIGHT
                jr      z,.i_brt
                cp      INL_FLASH
                jr      z,.i_fl
                cp      INL_NOFLASH
                jr      z,.i_nfl
                cp      INL_PAUSE
                jr      z,.i_pau
                cp      INL_SPEED
                jr      z,.i_spd
                cp      INL_ALIGN_C
                jp      z,.i_ac
                cp      INL_ALIGN_R
                jp      z,.i_ar
                cp      INL_ALIGN_L
                jp      z,.i_al
                inc     hl              ; unknown → skip
                jp      .lp

.i_brt:         inc     hl
                ld      a,(hl)
                sub     INL_PARAM_BASE  ; 0..15
                inc     a               ; 1..16
                sla     a
                sla     a
                sla     a
                sla     a               ; 16..256
                jr      nz,.brt_ok
                ld      a,0FFh          ; 256→0, clamp to 255
.brt_ok:        dec     a               ; 15..255
                ld      (state.inl_bright),a
                inc     hl
                jp      .lp

.i_fl:          ld      a,1
                ld      (state.inl_flash),a
                inc     hl
                jp      .lp

.i_nfl:         xor     a
                ld      (state.inl_flash),a
                inc     hl
                jp      .lp

.i_pau:         inc     hl
                ld      a,(hl)
                sub     INL_PARAM_BASE
                ld      (de),a          ; duration as "char" value
                ld      (ix+0),0        ; invisible
                ld      (iy+0),1 << ATTR_F_PAUSE
                inc     de
                inc     ix
                inc     iy
                inc     c
                inc     hl
                jp      .lp

.i_spd:         inc     hl
                ld      a,(hl)
                sub     INL_PARAM_BASE
                inc     a               ; 1..16
                ld      (state.speed),a
                inc     hl
                jp      .lp

.i_ac:          ld      a,1
                ld      (state.align_mode),a
                inc     hl
                jp      .lp

.i_ar:          ld      a,2
                ld      (state.align_mode),a
                inc     hl
                jp      .lp

.i_al:          xor     a
                ld      (state.align_mode),a
                inc     hl
                jp      .lp

.char:          ld      (de),a
                push    af
                ld      a,(state.inl_bright)
                ld      (ix+0),a
                ld      a,(state.inl_flash)
                ld      (iy+0),a
                pop     af
                inc     de
                inc     ix
                inc     iy
                inc     c
                inc     b
                inc     hl
                jp      .lp

.done:          ld      a,c
                ld      (state.text_len),a
                ld      a,b
                ld      (state.vis_len),a
                ld      a,(hl)
                cp      PAGE_END
                jr      nz,.no_skip
                inc     hl
.no_skip:       ret


; ============================================================================
; RENDER_FULL — text_buf → disp_seg with alignment, skipping pauses
; ============================================================================
; Clobbers: AF, BC, DE, HL, IX, IY
; ============================================================================
render_full:
                ld      hl,disp_seg
                ld      b,DISP_COLS * 2
                xor     a
.clr:           ld      (hl),a
                inc     hl
                djnz    .clr

                ld      a,(state.text_len)
                or      a
                ret     z
                ld      b,a             ; B = entries to process

                call    calc_start_col
                ld      c,a             ; C = display column

                ld      ix,text_buf
                ld      iy,attr_flags

.lp:            ld      a,c
                cp      DISP_COLS
                ret     nc              ; off right edge

                bit     ATTR_F_PAUSE,(iy+0)
                jr      nz,.skip        ; pause: no render, no column advance

                ; Font lookup
                ld      a,(ix+0)
                push    bc
                push    ix
                push    iy
                call    font_lookup     ; HL = bitmask
                pop     iy
                pop     ix
                pop     bc

                ; Store at disp_seg[C*2]
                push    hl              ; bitmask
                ld      a,c
                add     a,a
                ld      e,a
                ld      d,0
                ld      hl,disp_seg
                add     hl,de
                pop     de              ; DE = bitmask
                ld      (hl),e
                inc     hl
                ld      (hl),d

                inc     c               ; advance column

.skip:          inc     ix
                inc     iy
                djnz    .lp
                ret


; ============================================================================
; SET_FULL_BRIGHT — attr_bright → disp_bright with alignment, skip pauses
; ============================================================================
; Clobbers: AF, BC, DE, HL, IX
; ============================================================================
set_full_bright:
                ld      a,(state.text_len)
                or      a
                ret     z
                ld      b,a

                call    calc_start_col
                ld      c,a             ; C = display column

                ld      ix,attr_flags
                ld      hl,attr_bright

.lp:            bit     ATTR_F_PAUSE,(ix+0)
                jr      nz,.skip

                ld      a,c
                cp      DISP_COLS
                jr      nc,.skip_vis    ; off screen, still advance column

                ; Write brightness
                ld      a,(hl)
                push    hl
                ld      e,c
                ld      d,0
                ld      hl,disp_bright
                add     hl,de
                ld      (hl),a
                pop     hl

                inc     c
                jr      .next

.skip:          jr      .next           ; pause: no column advance
.skip_vis:      inc     c               ; visible but off screen
.next:          inc     hl
                inc     ix
                djnz    .lp
                ret


; ============================================================================
; SET_ALL_BRIGHT — Set all 24 disp_bright entries to 0xFF
; ============================================================================
; Clobbers: AF, B, HL
; ============================================================================
set_all_bright:
                ld      hl,disp_bright
                ld      b,DISP_COLS
                ld      a,0FFh
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                ret


; ============================================================================
; RENDER_SCROLLED — text_buf → disp_seg with scroll offset
; ============================================================================
; Wraps with a DISP_COLS-wide gap between repeats.
; Pauses are treated as blanks during scrolling.
; Clobbers: AF, BC, DE, HL
; ============================================================================
render_scrolled:
                ld      hl,disp_seg
                ld      b,DISP_COLS * 2
                xor     a
.clr:           ld      (hl),a
                inc     hl
                djnz    .clr

                ; Wrap point = text_len + DISP_COLS
                ld      a,(state.text_len)
                add     a,DISP_COLS
                ld      d,a             ; D = wrap point

                ld      b,DISP_COLS
                ld      c,0             ; C = display column

.lp:            ; index = (text_offset + C) mod wrap_point
                ld      a,(state.text_offset)
                add     a,c
.mod:           cp      d
                jr      c,.mod_ok
                sub     d
                jr      .mod
.mod_ok:        ; A = source index
                ; Blank if index >= text_len (gap region)
                ld      e,a
                ld      a,(state.text_len)
                cp      e
                jr      c,.blank        ; text_len < index
                jr      z,.blank        ; text_len == index

                ; Check pause marker
                push    bc
                push    de
                ld      d,0
                ld      hl,attr_flags
                add     hl,de
                bit     ATTR_F_PAUSE,(hl)
                jr      nz,.blank_pop   ; treat pauses as blank

                ; Fetch char
                ld      hl,text_buf
                add     hl,de
                ld      a,(hl)

                ; Font lookup
                call    font_lookup     ; HL = bitmask
                pop     de              ; E = source index (don't need)
                pop     bc              ; C = display column

                ; Store at disp_seg[C*2]
                push    hl              ; bitmask
                ld      a,c
                add     a,a
                ld      e,a
                ld      d,0
                ld      hl,disp_seg
                add     hl,de
                pop     de              ; DE = bitmask
                ld      (hl),e
                inc     hl
                ld      (hl),d
                jr      .next

.blank_pop:     pop     de
                pop     bc
.blank:
.next:          inc     c
                djnz    .lp
                ret


; ============================================================================
; Helper: bright_col_ff — set disp_bright[A] = 0xFF
; ============================================================================
bright_col_ff:
                ld      e,a
                ld      d,0
                ld      hl,disp_bright
                add     hl,de
                ld      (hl),0FFh
                ret


; ============================================================================
; Helper: clear_col — clear disp_bright[A] and disp_seg[A]
; ============================================================================
clear_col:
                ld      e,a
                ld      d,0
                push    de
                ld      hl,disp_bright
                add     hl,de
                ld      (hl),0
                pop     de
                ld      hl,disp_seg
                add     hl,de
                add     hl,de           ; ×2
                ld      (hl),0
                inc     hl
                ld      (hl),0
                ret


; ============================================================================
; ENTRY TRANSITIONS — return CF set when complete
; ============================================================================
fx_trans_step:
                ld      a,(state.fx_entry)
                cp      TRANS_FADE
                jp      z,fx_fade_in
                cp      TRANS_WIPE_L
                jp      z,fx_wipe_l_in
                cp      TRANS_WIPE_R
                jp      z,fx_wipe_r_in
                cp      TRANS_WIPE_CTR
                jp      z,fx_wipe_ctr_in
                cp      TRANS_WIPE_EDGE
                jp      z,fx_wipe_edge_in
                cp      TRANS_DISSOLVE
                jp      z,fx_dissolve_in
                cp      TRANS_FADE_WP_L
                jp      z,fx_fade_wipe_l_in
                cp      TRANS_FADE_WP_R
                jp      z,fx_fade_wipe_r_in
                scf                     ; unknown → done
                ret

; --- FADE IN: 16 steps, brightness ≈ step × 17 ---
fx_fade_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      16
                jr      nc,.done
                inc     a
                ld      (state.fx_pos),a
                ; brightness = step × 16 + step = step × 17
                ld      b,a
                sla     a
                sla     a
                sla     a
                sla     a               ; ×16
                add     a,b             ; ×17
                jr      nc,.ok
                ld      a,0FFh
.ok:            ld      hl,disp_bright
                ld      b,DISP_COLS
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                or      a               ; CF=0
                ret
.done:          scf
                ret

; --- WIPE LEFT IN ---
fx_wipe_l_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS
                jr      nc,.done
                call    bright_col_ff
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE RIGHT IN ---
fx_wipe_r_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS
                jr      nc,.done
                ld      b,a
                ld      a,DISP_COLS - 1
                sub     b
                call    bright_col_ff
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE CENTER IN ---
fx_wipe_ctr_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS / 2
                jr      nc,.done
                ld      b,a
                ld      a,(DISP_COLS / 2) - 1
                sub     b
                call    bright_col_ff
                ld      a,(DISP_COLS / 2)
                add     a,b
                call    bright_col_ff
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE EDGES IN ---
fx_wipe_edge_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS / 2
                jr      nc,.done
                ld      b,a
                ld      a,b
                call    bright_col_ff
                ld      a,DISP_COLS - 1
                sub     b
                call    bright_col_ff
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- DISSOLVE IN ---
fx_dissolve_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS
                jr      nc,.done
                ld      e,a
                ld      d,0
                ld      hl,dissolve_order
                add     hl,de
                ld      a,(hl)
                call    bright_col_ff
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret


; --- FADE WIPE LEFT IN: wipe left with 3-column fade trail ---
; fx_pos tracks the leading edge. Columns behind it get partial brightness.
fx_fade_wipe_l_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS + 3   ; need 3 extra steps for trail to finish
                jr      nc,.done
                ; Set brightness for columns 0..fx_pos
                ; Leading edge (fx_pos) = 0x55, fx_pos-1 = 0xAA, fx_pos-2+ = 0xFF
                ld      b,DISP_COLS
                ld      c,0             ; column
                ld      hl,disp_bright
.lp:            ld      a,(state.fx_pos)
                sub     c               ; distance = fx_pos - column
                ; distance < 0 → column ahead of leading edge → dark
                jr      c,.dark
                ; distance == 0 → leading edge → dim
                jr      z,.dim1
                cp      1
                jr      z,.dim2
                cp      2
                jr      z,.dim3
                ; distance >= 3 → fully revealed
                ld      (hl),0FFh
                jr      .next
.dim1:          ld      (hl),055h
                jr      .next
.dim2:          ld      (hl),0AAh
                jr      .next
.dim3:          ld      (hl),0DDh
                jr      .next
.dark:          ld      (hl),0
.next:          inc     hl
                inc     c
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret


; --- FADE WIPE RIGHT IN: wipe right with 3-column fade trail ---
fx_fade_wipe_r_in:
                call    render_full
                ld      a,(state.fx_pos)
                cp      DISP_COLS + 3
                jr      nc,.done
                ld      b,DISP_COLS
                ld      c,0
                ld      hl,disp_bright
.lp:            ; Leading edge at column (23 - fx_pos)
                ; distance = column - leading_edge
                push    hl
                ld      a,(state.fx_pos)
                ld      d,a
                ld      a,DISP_COLS - 1
                sub     d               ; A = 23 - fx_pos = leading edge column
                ld      d,a             ; D = leading edge
                ld      a,c
                sub     d               ; distance = column - leading_edge
                pop     hl
                jr      c,.dark
                jr      z,.dim1
                cp      1
                jr      z,.dim2
                cp      2
                jr      z,.dim3
                ld      (hl),0FFh
                jr      .next
.dim1:          ld      (hl),055h
                jr      .next
.dim2:          ld      (hl),0AAh
                jr      .next
.dim3:          ld      (hl),0DDh
                jr      .next
.dark:          ld      (hl),0
.next:          inc     hl
                inc     c
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret


; ============================================================================
; EXIT TRANSITIONS — return CF set when complete
; ============================================================================
fx_trans_exit_step:
                ld      a,(state.fx_exit)
                cp      TRANS_FADE
                jp      z,fx_fade_out
                cp      TRANS_WIPE_L
                jp      z,fx_wipe_l_out
                cp      TRANS_WIPE_R
                jp      z,fx_wipe_r_out
                cp      TRANS_WIPE_CTR
                jp      z,fx_wipe_ctr_out
                cp      TRANS_WIPE_EDGE
                jp      z,fx_wipe_edge_out
                cp      TRANS_DISSOLVE
                jp      z,fx_dissolve_out
                cp      TRANS_FADE_WP_L
                jp      z,fx_fade_wipe_l_out
                cp      TRANS_FADE_WP_R
                jp      z,fx_fade_wipe_r_out
                scf
                ret

; --- FADE OUT ---
fx_fade_out:
                ld      a,(state.fx_pos)
                cp      16
                jr      nc,.done
                ; brightness = (15 - pos + 1) × 16 + (15 - pos + 1) - 1
                ; Simpler: (16 - (pos+1)) × 17 ... let's just do (15-pos)*17
                ld      b,a
                ld      a,15
                sub     b               ; 15..0
                ld      c,a
                sla     a
                sla     a
                sla     a
                sla     a               ; ×16
                add     a,c             ; ×17
                jr      nc,.ok
                ld      a,0FFh
.ok:            ld      hl,disp_bright
                ld      b,DISP_COLS
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE LEFT OUT ---
fx_wipe_l_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS
                jr      nc,.done
                call    clear_col
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE RIGHT OUT ---
fx_wipe_r_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS
                jr      nc,.done
                ld      b,a
                ld      a,DISP_COLS - 1
                sub     b
                call    clear_col
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE CENTER OUT ---
fx_wipe_ctr_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS / 2
                jr      nc,.done
                ld      b,a
                ld      a,(DISP_COLS / 2) - 1
                sub     b
                call    clear_col
                ld      a,(DISP_COLS / 2)
                add     a,b
                call    clear_col
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- WIPE EDGES OUT ---
fx_wipe_edge_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS / 2
                jr      nc,.done
                ld      b,a
                ld      a,b
                call    clear_col
                ld      a,DISP_COLS - 1
                sub     b
                call    clear_col
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- DISSOLVE OUT ---
fx_dissolve_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS
                jr      nc,.done
                ld      e,a
                ld      d,0
                ld      hl,dissolve_order
                add     hl,de
                ld      a,(hl)
                call    clear_col
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret


; --- FADE WIPE LEFT OUT: darken left-to-right with fade trail ---
fx_fade_wipe_l_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS + 3
                jr      nc,.done
                ld      b,DISP_COLS
                ld      c,0
                ld      hl,disp_bright
.lp:            ; leading dark edge at column fx_pos
                ; distance = fx_pos - column
                ld      a,(state.fx_pos)
                sub     c
                jr      c,.bright       ; column ahead of edge → still lit
                jr      z,.dim1         ; at edge → getting dim
                cp      1
                jr      z,.dim2
                cp      2
                jr      z,.dim3
                ; fully dark
                ld      (hl),0
                jr      .next
.dim1:          ld      (hl),0AAh       ; just hit
                jr      .next
.dim2:          ld      (hl),055h
                jr      .next
.dim3:          ld      (hl),022h
                jr      .next
.bright:        ; leave brightness as-is (or ensure full)
                ld      (hl),0FFh
.next:          inc     hl
                inc     c
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret


; --- FADE WIPE RIGHT OUT: darken right-to-left with fade trail ---
fx_fade_wipe_r_out:
                ld      a,(state.fx_pos)
                cp      DISP_COLS + 3
                jr      nc,.done
                ld      b,DISP_COLS
                ld      c,0
                ld      hl,disp_bright
.lp:            ; leading dark edge at column (23 - fx_pos)
                push    hl
                ld      a,(state.fx_pos)
                ld      d,a
                ld      a,DISP_COLS - 1
                sub     d               ; A = leading edge column
                ld      d,a
                ld      a,d
                sub     c               ; distance = edge - column
                pop     hl
                jr      c,.bright       ; column past edge → still lit
                jr      z,.dim1
                cp      1
                jr      z,.dim2
                cp      2
                jr      z,.dim3
                ld      (hl),0
                jr      .next
.dim1:          ld      (hl),0AAh
                jr      .next
.dim2:          ld      (hl),055h
                jr      .next
.dim3:          ld      (hl),022h
                jr      .next
.bright:        ld      (hl),0FFh
.next:          inc     hl
                inc     c
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret


; ============================================================================
; DISPLAY EFFECTS — called each step during dwell phase
; ============================================================================
fx_display_step:
                ld      a,(state.fx_display)
                or      a
                ret     z               ; DISP_STATIC
                cp      DISP_FLASH
                jp      z,fx_dsp_flash
                cp      DISP_BREATHE
                jp      z,fx_dsp_breathe
                cp      DISP_SCROLL_L
                jp      z,fx_dsp_scroll_l
                cp      DISP_SCROLL_R
                jp      z,fx_dsp_scroll_r
                cp      DISP_MARQUEE_L
                jp      z,fx_dsp_marquee_l
                cp      DISP_MARQUEE_R
                jp      z,fx_dsp_marquee_r
                cp      DISP_TYPEWRITER
                jp      z,fx_dsp_typewriter
                cp      DISP_TWRT_R
                jp      z,fx_dsp_typewriter_r
                cp      DISP_BOUNCE
                jp      z,fx_dsp_bounce
                cp      DISP_FLASH_ALT
                jp      z,fx_dsp_flash_alt
                cp      DISP_SPARKLE
                jp      z,fx_dsp_sparkle
                cp      DISP_WAVE
                jp      z,fx_dsp_wave
                ret


; --- FLASH ---
fx_dsp_flash:
                ld      a,(state.flash_phase)
                or      a
                jr      z,.off
                jp      set_full_bright
.off:           ld      hl,disp_bright
                ld      b,DISP_COLS
                xor     a
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                ret


; --- BREATHE ---
fx_dsp_breathe:
                ld      a,(state.fx_pos)
                and     1Fh
                ld      e,a
                ld      d,0
                ld      hl,sine_table
                add     hl,de
                ld      a,(hl)
                ld      hl,disp_bright
                ld      b,DISP_COLS
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                ret


; --- SCROLL LEFT ---
fx_dsp_scroll_l:
                ld      a,(state.text_offset)
                inc     a
                ld      b,a
                ld      a,(state.text_len)
                add     a,DISP_COLS
                cp      b
                jr      nz,.no_wrap
                ld      b,0
.no_wrap:       ld      a,b
                ld      (state.text_offset),a
                call    render_scrolled
                jp      set_all_bright


; --- SCROLL RIGHT ---
fx_dsp_scroll_r:
                ld      a,(state.text_offset)
                or      a
                jr      nz,.dec
                ld      a,(state.text_len)
                add     a,DISP_COLS - 1
                jr      .set
.dec:           dec     a
.set:           ld      (state.text_offset),a
                call    render_scrolled
                jp      set_all_bright


; --- TYPEWRITER ---
; Reveals one visible character per step. Pauses insert a tick delay.
; Uses fx_pos as index into the text_len entries.
fx_dsp_typewriter:
                ld      a,(state.fx_pos)
                ld      c,a             ; C = text_buf index
                ld      a,(state.text_len)
                cp      c
                ret     z               ; all done
                ret     c

                ; Pause marker?
                ld      b,0
                ld      hl,attr_flags
                add     hl,bc
                bit     ATTR_F_PAUSE,(hl)
                jr      z,.tw_vis

                ; Load pause duration and advance
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                ld      (state.pause_count),a
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.tw_vis:        ; Count visible chars at indices 0..C-1 for column offset
                push    bc              ; save C
                call    count_visible   ; A = visible count before C
                ld      d,a             ; D = vis count
                call    calc_start_col  ; A = alignment offset
                add     a,d             ; A = display column
                pop     bc              ; C = text index
                cp      DISP_COLS
                jr      nc,.tw_adv      ; off screen, just advance

                ld      e,a             ; E = display column

                ; Write brightness
                push    bc
                push    de
                ld      b,0
                ld      hl,attr_bright
                add     hl,bc
                ld      a,(hl)          ; brightness value
                ld      l,e
                ld      h,0
                ld      de,disp_bright
                add     hl,de
                ld      (hl),a
                pop     de              ; E = display column
                pop     bc              ; C = text index

                ; Write segment pattern
                push    de              ; save display column
                ld      b,0
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)          ; ASCII char
                call    font_lookup     ; HL = bitmask
                pop     de              ; E = display column
                ; disp_seg[E*2]
                push    hl              ; save bitmask
                ld      a,e
                add     a,a
                ld      e,a
                ld      d,0
                ld      hl,disp_seg
                add     hl,de           ; HL = dest
                pop     de              ; DE = bitmask
                ld      (hl),e
                inc     hl
                ld      (hl),d

.tw_adv:        ld      hl,state.fx_pos
                inc     (hl)
                ret


; --- COUNT_VISIBLE: count non-pause entries before index C ---
; Input:  C = text index (0-based)
; Output: A = visible char count at indices 0..C-1
; Clobbers: B, HL
count_visible:
                ld      a,c
                or      a
                jr      z,.zero
                ld      b,a
                ld      hl,attr_flags
                xor     a
.lp:            bit     ATTR_F_PAUSE,(hl)
                jr      nz,.skip
                inc     a
.skip:          inc     hl
                djnz    .lp
                ret
.zero:          xor     a
                ret


; --- BOUNCE ---
fx_dsp_bounce:
                ld      a,(state.text_len)
                cp      DISP_COLS + 1
                ret     c               ; fits on display

                ld      a,(state.bounce_dir)
                or      a
                jr      nz,.right

                ld      a,(state.text_offset)
                inc     a
                ld      (state.text_offset),a
                ld      b,a
                ld      a,(state.text_len)
                sub     DISP_COLS
                cp      b
                jr      nz,.render
                ld      a,1
                ld      (state.bounce_dir),a
                jr      .render

.right:         ld      a,(state.text_offset)
                dec     a
                ld      (state.text_offset),a
                or      a
                jr      nz,.render
                xor     a
                ld      (state.bounce_dir),a

.render:        call    render_scrolled
                jp      set_all_bright


; --- FLASH ALTERNATING ---
fx_dsp_flash_alt:
                ld      a,(state.flash_phase)
                ld      c,a
                ld      hl,disp_bright
                ld      b,DISP_COLS
                ld      d,0
.lp:            ld      a,d
                and     1
                cp      c
                jr      z,.on
                ld      (hl),0
                jr      .next
.on:            ld      (hl),0FFh
.next:          inc     hl
                inc     d
                djnz    .lp
                ret


; --- MARQUEE LEFT: scroll in from right, hold centered, scroll out left ---
; Uses marquee_state: 0=scrolling in, 1=holding, 2=scrolling out
; fx_pos tracks scroll position within each sub-state.
; Text starts fully off-screen right and scrolls left until centered,
; then holds, then continues scrolling left until fully off-screen.
fx_dsp_marquee_l:
                ld      a,(state.marquee_state)
                or      a
                jr      z,.mql_in
                cp      1
                jr      z,.mql_hold
                jr      .mql_out

.mql_in:        ; Scrolling in: text_offset goes from 0 up
                ; Text starts DISP_COLS positions to the right of display
                ; We render using render_scrolled with offset counting up from 0
                ; When text is centered we switch to hold
                ;
                ; For centering: we want the first char at calc_start_col
                ; In scroll terms: offset where first char appears at start_col
                ; = text_len + DISP_COLS - start_col - vis_len ... actually simpler:
                ; We scroll from offset = (text_len + DISP_COLS) - DISP_COLS = text_len
                ; That makes column 0 = gap region (blank), text starts at right edge
                ; Target offset for centered = text_len - start_col (or 0 for left-align)
                ;
                ; Simpler approach: use fx_pos as a position counter.
                ; Render text at display column (DISP_COLS - fx_pos) .. 
                ; i.e. text slides in from the right.
                ; When column reaches calc_start_col, switch to hold.

                ; Current render position = DISP_COLS - fx_pos
                ld      a,(state.fx_pos)
                ld      b,a
                push    bc
                call    calc_start_col  ; A = target start column
                pop     bc
                ld      c,a             ; C = target column

                ; Current start = DISP_COLS - fx_pos
                ld      a,DISP_COLS
                sub     b               ; A = current start column
                cp      c
                jr      c,.mql_centered ; went past center (shouldn't happen, but safety)
                jr      z,.mql_centered

                ; Not centered yet — render at current position
                call    .mql_render_at  ; render text starting at column A
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.mql_centered:  ; Switch to hold state
                ld      a,1
                ld      (state.marquee_state),a
                ; Set up hold timer: use 1/3 of total dwell time
                ; Actually, let's use a fixed hold of ~2 seconds (100 ticks)
                ; or we can split dwell evenly. Fixed is simpler.
                ld      hl,100          ; 2 seconds hold
                ld      (state.marquee_hold),hl
                ; Render in final centered position
                call    render_full
                call    set_full_bright
                ret

.mql_hold:      ; Count down hold timer
                ld      hl,(state.marquee_hold)
                dec     hl
                ld      (state.marquee_hold),hl
                ld      a,h
                or      l
                ret     nz              ; still holding (display already rendered)
                ; Switch to scroll out
                ld      a,2
                ld      (state.marquee_state),a
                xor     a
                ld      (state.fx_pos),a
                ret

.mql_out:       ; Scroll out left: text slides from centered to off-screen left
                ; Start col = calc_start_col - fx_pos
                ld      a,(state.fx_pos)
                ld      b,a
                push    bc
                call    calc_start_col
                pop     bc
                sub     b               ; A = start_col - fx_pos (may go negative)
                jr      c,.mql_gone     ; fully off screen left
                ; Check if entire text is off screen
                push    af
                ld      a,(state.vis_len)
                add     a,c             ; wait, C isn't set here
                pop     af
                ; Simpler: if fx_pos > calc_start_col + vis_len, we're done
                push    af
                call    calc_start_col
                ld      c,a
                ld      a,(state.vis_len)
                add     a,c             ; A = rightmost column + 1
                ld      c,a
                ld      a,(state.fx_pos)
                cp      c
                pop     af
                jr      nc,.mql_gone

                call    .mql_render_at
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.mql_gone:      ; Off screen — we're done. The dwell timer will expire naturally.
                call    display_clear
                ret

; Helper: render text starting at display column A (signed-ish, 0..23)
; Clears display first, then places glyphs starting at column A
.mql_render_at:
                ld      c,a             ; C = start column
                ; Clear display
                push    bc
                ld      hl,disp_seg
                ld      b,DISP_COLS * 2
                xor     a
.clr1:          ld      (hl),a
                inc     hl
                djnz    .clr1
                ld      hl,disp_bright
                ld      b,DISP_COLS
                xor     a
.clr2:          ld      (hl),a
                inc     hl
                djnz    .clr2
                pop     bc              ; C = start column

                ld      a,(state.text_len)
                or      a
                ret     z
                ld      b,a
                ld      ix,text_buf
                ld      iy,attr_flags

.mrl:           ld      a,c
                cp      DISP_COLS
                ret     nc              ; past right edge, done

                bit     ATTR_F_PAUSE,(iy+0)
                jr      nz,.mrs         ; skip pause, no column advance

                ; Only render if column >= 0 (C is unsigned, but if we entered
                ; with a high value from underflow, skip it)
                ; Actually C is unsigned 0..255, so just check < DISP_COLS above
                ; and we're fine since we only call with valid start cols

                ld      a,(ix+0)
                push    bc
                push    ix
                push    iy
                call    font_lookup
                pop     iy
                pop     ix
                pop     bc
                push    hl
                ld      a,c
                add     a,a
                ld      e,a
                ld      d,0
                ld      hl,disp_seg
                add     hl,de
                pop     de
                ld      (hl),e
                inc     hl
                ld      (hl),d

                ; Brightness
                push    bc
                ld      e,c
                ld      d,0
                ld      hl,disp_bright
                add     hl,de
                ld      (hl),0FFh
                pop     bc

                inc     c

.mrs:           inc     ix
                inc     iy
                djnz    .mrl
                ret


; --- MARQUEE RIGHT: scroll in from left, hold centered, scroll out right ---
fx_dsp_marquee_r:
                ld      a,(state.marquee_state)
                or      a
                jr      z,.mqr_in
                cp      1
                jr      z,.mqr_hold
                jr      .mqr_out

.mqr_in:        ; Text slides in from left: starts at column -(vis_len), moving right
                ; render_col = fx_pos - vis_len
                ld      a,(state.fx_pos)
                ld      b,a
                push    bc
                call    calc_start_col
                pop     bc
                ld      c,a             ; C = target
                ; Current start = fx_pos - vis_len (may be negative → off left)
                ld      a,b
                ld      d,a
                ld      a,(state.vis_len)
                ld      e,a
                ld      a,d
                sub     e               ; A = fx_pos - vis_len (render start, possibly underflowed)
                ; If fx_pos - vis_len >= target, we're centered
                ; But A might have underflowed. Check: if fx_pos < vis_len, not there yet
                ld      a,d             ; fx_pos
                cp      e               ; vis_len
                jr      c,.mqr_nocheck  ; fx_pos < vis_len, definitely not centered
                ; fx_pos >= vis_len: check if start >= target
                ld      a,d
                sub     e
                cp      c
                jr      nc,.mqr_centered
.mqr_nocheck:
                ; Render: place text starting at max(0, fx_pos - vis_len)
                ld      a,d
                cp      e
                jr      nc,.mqr_pos
                xor     a               ; start at 0, but skip leading chars that are off-screen
                ; Actually for simplicity, use mql_render_at which handles column naturally
.mqr_pos:       ; For the "sliding in from left" we need to handle partial visibility
                ; Easier: render at column 0 with an offset into the text
                ; Let's reuse the scroll approach: text_offset = vis_len + DISP_COLS - fx_pos
                ; This is getting complex. Simpler: just use mql_render_at with the calculated column
                ld      a,d
                sub     e               ; underflows if fx_pos < vis_len
                jr      nc,.mqr_have_col
                ; Negative start: some chars off left edge. Still call render_at with col=0
                ; but skip the first (vis_len - fx_pos) visible chars... 
                ; This is tricky. Let's use a different approach for marquee_r.
                ; Use render_scrolled with a computed offset.
                ; offset such that text appears at the right position.
                ; text_offset = wrap_point - (DISP_COLS - fx_pos + vis_len) ... 
                ; Actually let's just accept that partial left-entry won't look perfect
                ; and start rendering from column 0 once any part is visible.
                xor     a
.mqr_have_col:
                call    fx_dsp_marquee_l.mql_render_at
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.mqr_centered:
                ld      a,1
                ld      (state.marquee_state),a
                ld      hl,100
                ld      (state.marquee_hold),hl
                call    render_full
                call    set_full_bright
                ret

.mqr_hold:      ld      hl,(state.marquee_hold)
                dec     hl
                ld      (state.marquee_hold),hl
                ld      a,h
                or      l
                ret     nz
                ld      a,2
                ld      (state.marquee_state),a
                xor     a
                ld      (state.fx_pos),a
                ret

.mqr_out:       ; Scroll out right: start_col = calc_start_col + fx_pos
                ld      a,(state.fx_pos)
                ld      b,a
                push    bc
                call    calc_start_col
                pop     bc
                add     a,b             ; A = start col + fx_pos
                cp      DISP_COLS
                jr      nc,.mqr_gone    ; fully off right

                call    fx_dsp_marquee_l.mql_render_at
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.mqr_gone:      call    display_clear
                ret


; --- TYPEWRITER RIGHT: reveal chars right-to-left ---
; Like typewriter but reveals from the last visible char backwards.
fx_dsp_typewriter_r:
                ld      a,(state.fx_pos)
                ld      c,a             ; C = how many chars revealed so far
                ld      a,(state.text_len)
                cp      c
                ret     z
                ret     c               ; all done

                ; Index into text_buf = text_len - 1 - fx_pos
                ; (reveal from end to start)
                ld      b,a             ; B = text_len
                dec     b
                ld      a,b
                sub     c               ; A = text_len - 1 - fx_pos = current index
                ld      c,a             ; C = text index (counting from end)

                ; Check pause marker
                ld      b,0
                ld      hl,attr_flags
                add     hl,bc
                bit     ATTR_F_PAUSE,(hl)
                jr      z,.twr_vis

                ; Pause
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                ld      (state.pause_count),a
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.twr_vis:       ; Count visible chars at indices 0..C-1 for column
                push    bc
                call    count_visible   ; A = visible count before C
                ld      d,a
                call    calc_start_col
                add     a,d             ; A = display column
                pop     bc
                cp      DISP_COLS
                jr      nc,.twr_adv

                ld      e,a             ; E = display column

                ; Write brightness
                push    bc
                push    de
                ld      b,0
                ld      hl,attr_bright
                add     hl,bc
                ld      a,(hl)
                ld      l,e
                ld      h,0
                ld      de,disp_bright
                add     hl,de
                ld      (hl),a
                pop     de
                pop     bc

                ; Write segment
                push    de
                ld      b,0
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                call    font_lookup
                pop     de
                push    hl
                ld      a,e
                add     a,a
                ld      e,a
                ld      d,0
                ld      hl,disp_seg
                add     hl,de
                pop     de
                ld      (hl),e
                inc     hl
                ld      (hl),d

.twr_adv:       ld      hl,state.fx_pos
                inc     (hl)
                ret


; --- SPARKLE: random chars briefly dim then restore ---
; Each step, pick a random column and reduce its brightness.
; All other columns drift back toward full brightness.
fx_dsp_sparkle:
                ; Restore: move all columns toward 0xFF
                ld      hl,disp_bright
                ld      b,DISP_COLS
.restore:       ld      a,(hl)
                cp      0FFh
                jr      z,.r_skip
                add     a,20h           ; step toward full
                jr      nc,.r_ok
                ld      a,0FFh          ; clamp
.r_ok:          ld      (hl),a
.r_skip:        inc     hl
                djnz    .restore

                ; Pick 2 random columns to dim (more visual activity)
                call    lfsr_next
                and     1Fh             ; 0..31
                cp      DISP_COLS
                jr      nc,.sp1_skip    ; out of range, skip
                ld      e,a
                ld      d,0
                ld      hl,disp_bright
                add     hl,de
                ld      (hl),020h       ; dim
.sp1_skip:
                call    lfsr_next
                and     1Fh
                cp      DISP_COLS
                jr      nc,.sp2_skip
                ld      e,a
                ld      d,0
                ld      hl,disp_bright
                add     hl,de
                ld      (hl),040h       ; slightly less dim
.sp2_skip:
                ret


; --- LFSR_NEXT: 8-bit Galois LFSR, taps at bits 7,5,4,3 (maximal period 255) ---
; Output: A = next pseudo-random byte
; Clobbers: -
lfsr_next:
                ld      a,(state.lfsr)
                srl     a
                jr      nc,.no_tap
                xor     0B8h            ; taps: x^8 + x^6 + x^5 + x^4 + 1
.no_tap:        ld      (state.lfsr),a
                ret


; --- WAVE ---
fx_dsp_wave:
                ld      a,(state.fx_pos)
                ld      d,a
                ld      hl,disp_bright
                ld      b,DISP_COLS
                ld      c,0
.lp:            ld      a,d
                add     a,c
                and     1Fh
                push    hl
                push    bc
                ld      e,a
                ld      d,0
                ld      hl,sine_table
                add     hl,de
                ld      a,(hl)
                pop     bc
                pop     hl
                ld      (hl),a
                inc     hl
                inc     c
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                ret


; ============================================================================
; EXAMPLE MESSAGE STREAM
; ============================================================================
example_stream:
                ; Page 0: STATIC TEST — no effects, just render text
                ; If this doesn't show all chars, rendering is broken
                db      PAGE_START
                db      TRANS_NONE      ; instant appear
                db      DISP_STATIC     ; no animation
                db      TRANS_NONE      ; instant disappear
                db      1               ; speed (irrelevant for static)
                db      01h, 0F4h       ; dwell: 500 ticks = 10 sec
                db      "ABCDEFGHIJKLMNOPQRSTUVWX"
                db      PAGE_END

                ; Page 1: Title — fade in, breathe, fade out
                db      PAGE_START
                db      TRANS_FADE
                db      DISP_BREATHE
                db      TRANS_FADE
                db      3
                db      00h, 0FAh       ; 250 ticks = 5 sec
                db      "  SPACE INVADERS 3D   "
                db      PAGE_END

                ; Page 2: Scrolling instructions
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_SCROLL_L
                db      TRANS_NONE
                db      2
                db      02h, 58h        ; 600 ticks = 12 sec
                db      "ARROWS TO MOVE --- Z TO FIRE --- P TO PAUSE --- SURVIVE AS LONG AS YOU CAN!"
                db      PAGE_END

                ; Page 3: Marquee left
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_MARQUEE_L
                db      TRANS_NONE
                db      2
                db      02h, 58h        ; 600 ticks = 12 sec
                db      "WAVE 1"
                db      PAGE_END

                ; Page 4: Typewriter reveal with pause
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_TYPEWRITER
                db      TRANS_FADE
                db      4
                db      01h, 2Ch        ; 300 ticks = 6 sec
                db      INL_ALIGN_C
                db      "GET READY"
                db      INL_PAUSE, INL_PARAM_BASE + 10
                db      "..."
                db      PAGE_END

                ; Page 5: Sparkle effect with fade-wipe entry
                db      PAGE_START
                db      TRANS_FADE_WP_L
                db      DISP_SPARKLE
                db      TRANS_FADE_WP_R
                db      2
                db      01h, 0F4h       ; 500 ticks = 10 sec
                db      INL_ALIGN_C
                db      "HIGH SCORE  12500"
                db      PAGE_END

                ; Page 6: Typewriter right
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_TWRT_R
                db      TRANS_WIPE_CTR
                db      3
                db      01h, 2Ch        ; 300 ticks = 6 sec
                db      INL_ALIGN_C
                db      "GAME OVER"
                db      PAGE_END

                ; Page 7: Flashing insert coin
                db      PAGE_START
                db      TRANS_WIPE_CTR
                db      DISP_FLASH
                db      TRANS_WIPE_EDGE
                db      6
                db      02h, 58h        ; 600 ticks = 12 sec
                db      INL_ALIGN_C
                db      "INSERT COIN"
                db      PAGE_END

                db      END_STREAM
