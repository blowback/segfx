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
MAX_BRIGHT      equ     128             ; hardware ceiling — values above this are indistinguishable

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
TRANS_VFADE_D   equ     0Ah             ; vertical fade down (top row disappears first) — OUT
TRANS_VFADE_U   equ     0Bh             ; vertical fade up (bottom row disappears first) — OUT

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
INL_WAVE_TBL    equ     88h             ; next byte (INL_PARAM_BASE+n) selects wave table
INL_GLYPH       equ     89h             ; next 2 bytes = raw segment bitmask, rendered as one char
INL_CHARDEF     equ     8Ah             ; next byte = char code (A0-FE), then 2 bytes = segment pattern
INL_WAVE_DIR    equ     8Bh             ; next byte: 90h=L→R, 91h=R→L, 92h=bounce

; Custom character table
MAX_CUSTOM_CHARS equ    16              ; max custom glyph definitions per page

; Inline parameter range
INL_PARAM_BASE  equ     90h             ; 90h..9Fh encode values 0..15

; Attribute flag bits
ATTR_F_FLASH    equ     0               ; bit 0: per-char flash
ATTR_F_SPEED    equ     6               ; bit 6: speed change marker
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
wave_tbl        db      0               ; wave table index (0=sine, 1=glint, ...)
wave_dir        db      0               ; wave direction: 0=L→R, 1=R→L, 2=bounce
glyph_code      db      0FEh            ; next auto-assigned code for INL_GLYPH (counts down from 0xFE)
page_index      db      0               ; current page number (0-based)
callback        dw      0               ; user callback address (0 = disabled)
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
disp_flash      ds      DISP_COLS       ; 24 bytes: per-column flash flag (0=solid, 1=flash)

; Text workspace
text_buf        ds      256             ; decoded ASCII text
attr_bright     ds      256             ; per-char brightness
attr_flags      ds      256             ; per-char flags (bit 0=flash, bit 6=speed, bit 7=pause)

; Shadow buffer for segment-masking transitions (vfade in)
shadow_seg      ds      DISP_COLS * 2   ; 48 bytes: copy of pre-rendered segments

; Custom character definitions (INL_CHARDEF)
; Each entry: 1 byte code (0xA0-0xFE) + 2 bytes segment pattern = 3 bytes
custom_chars    ds      MAX_CUSTOM_CHARS * 3    ; 48 bytes
custom_count    db      0                       ; number of defined custom chars


; ============================================================================
; ROM data
; ============================================================================

; Vertical segment row masks (top to bottom, 5 rows)
; Row 0 (top):    a1,a2        = 0x0001
; Row 1 (upper):  f,g1,g2,b   = 0x0722
; Row 2 (middle): e,d2         = 0x00C0  (wait, let me recalculate)
; Actually using the user's exact values:
; Row 0 (top):    1                          = 0x0001
; Row 1:          32,256,512,1024,2           = 0x0722
; Row 2 (middle): 64,128                     = 0x00C0
; Row 3:          16,8192,4096,2048,4         = 0x3814
; Row 4 (bottom): 8,16384                    = 0x4008
;
; For OUT (mask off): AND with NOT(mask) at each step
; For IN (mask on):   start blank, OR in mask at each step
vfade_row_masks:
                dw      0001h           ; row 0 (top)
                dw      0722h           ; row 1
                dw      00C0h           ; row 2 (middle)
                dw      3814h           ; row 3
                dw      4008h           ; row 4 (bottom)

; Dissolve order: pseudo-random column reveal sequence
dissolve_order:
                db      11,3,19,7,15,0,22,5,17,9,13,2,20,6,23,10,16,1,21,8,14,4,18,12

; Wave tables: 32 entries each, full cycle, indexed by state.wave_tbl
; All tables must be 32 bytes and contiguous.
WAVE_TBL_SIZE   equ     32

wave_tables:

; Table 0: Sine — smooth full-range oscillation (original)
wave_tbl_sine:
                db      80h,98h,0B0h,0C6h,0DAh,0EAh,0F6h,0FDh
                db      0FFh,0FDh,0F6h,0EAh,0DAh,0C6h,0B0h,98h
                db      80h,67h,4Fh,39h,25h,15h,09h,02h
                db      00h,02h,09h,15h,25h,39h,4Fh,67h

; Table 1: Glint — narrow bright highlight sweeping across dim text
wave_tbl_glint:
                db      18h,18h,18h,18h,18h,18h,18h,18h
                db      18h,18h,18h,20h,40h,80h,0FFh,0FFh
                db      80h,40h,20h,18h,18h,18h,18h,18h
                db      18h,18h,18h,18h,18h,18h,18h,18h

; Table 2: Pulse — sharp on/off square-ish wave
wave_tbl_pulse:
                db      0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh
                db      0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh
                db      10h,10h,10h,10h,10h,10h,10h,10h
                db      10h,10h,10h,10h,10h,10h,10h,10h

; Table 3: Sawtooth — ramp up then drop
wave_tbl_saw:
                db      00h,08h,10h,18h,20h,28h,30h,38h
                db      40h,48h,50h,58h,60h,68h,70h,78h
                db      80h,88h,90h,98h,0A0h,0A8h,0B0h,0B8h
                db      0C0h,0C8h,0D0h,0D8h,0E0h,0E8h,0F0h,0F8h

; Table 4: Flame — flickery randomish low-mid brightness
wave_tbl_flame:
                db      30h,60h,20h,50h,80h,40h,70h,30h
                db      90h,50h,20h,60h,0A0h,40h,80h,30h
                db      70h,50h,0B0h,40h,60h,20h,90h,50h
                db      30h,80h,40h,60h,20h,70h,50h,40h

WAVE_TBL_COUNT  equ     5

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
                ; Check if this is a custom character (>= 0xA0)
                cp      0A0h
                jr      nc,.fl_custom
                ; Standard ASCII: index into font_table
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

.fl_custom:     ; Search custom_chars table for matching code
                push    bc
                ld      c,a             ; C = target code
                ld      a,(custom_count)
                or      a
                jr      z,.fl_notfound
                ld      b,a             ; B = count
                ld      hl,custom_chars
.fl_search:     ld      a,(hl)          ; code
                cp      c
                jr      z,.fl_found
                inc     hl
                inc     hl
                inc     hl              ; next entry (3 bytes)
                djnz    .fl_search
.fl_notfound:   ; Not found — return blank
                pop     bc
                ld      hl,0
                ret
.fl_found:      inc     hl
                ld      a,(hl)          ; seg low
                inc     hl
                ld      h,(hl)          ; seg high
                ld      l,a
                pop     bc
                ret


; ============================================================================
; CHAR_TO_SEGMENTS — Get segment bitmask for current character
; ============================================================================
; Input:  IX = pointer to text_buf entry
;         IY = pointer to attr_flags entry
; Output: HL = segment bitmask
; Clobbers: AF, DE
; Note: Caller must push/pop BC, IX, IY around this call
; ============================================================================
char_to_segments:
                ld      a,(ix+0)
                jp      font_lookup     ; handles both standard and custom chars


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
                ld      (state.page_index),a
                ld      (state.callback),a
                ld      (state.callback+1),a
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
                ld      hl,disp_seg     ; disp_seg, disp_bright, disp_flash are contiguous
                ld      b,DISP_COLS * 2 + DISP_COLS + DISP_COLS
                xor     a
.lp:            ld      (hl),a
                inc     hl
                djnz    .lp
                ret


; ============================================================================
; SEGFX_OUTPUT — Write display buffers to hardware via BIOS
; ============================================================================
; Uses explicit column counter (C) since BIOS calls may destroy A.
; Interrupts disabled to prevent tick handler modifying buffers mid-write.
; ============================================================================
segfx_output:
                di
                ; Pass 1: Segment patterns
                ld      ix,disp_seg
                ld      c,0             ; C = column counter
                ld      b,DISP_COLS
.seg:           push    bc
                ld      a,c             ; A = column
                ld      l,(ix+0)
                ld      h,(ix+1)
                call    BIOS_SEG        ; A=column, HL=bitmask
                inc     ix
                inc     ix
                pop     bc
                inc     c
                djnz    .seg

                ; Pass 2: Brightness (scaled to MAX_BRIGHT, with per-char flash)
                ld      ix,disp_bright
                ld      iy,disp_flash
                ld      c,0             ; C = column counter
                ld      b,DISP_COLS
.brt:           push    bc
                ld      a,c             ; A = column
                ld      e,a             ; E = column
                ld      a,(ix+0)        ; A = raw brightness 0..255
                ; Check per-column flash: if flash flag set and flash_phase=0, force dark
                bit     0,(iy+0)
                jr      z,.brt_nofl     ; not a flashing column
                push    af
                ld      a,(state.flash_phase)
                or      a
                jr      nz,.brt_fl_on
                pop     af
                xor     a               ; force brightness 0
                jr      .brt_scale
.brt_fl_on:     pop     af              ; restore original brightness
.brt_nofl:
.brt_scale:     srl     a               ; A = brightness >> 1 (0..127) for MAX_BRIGHT=128
                ld      c,a             ; C = scaled brightness
                ld      a,e             ; A = column
                call    BIOS_BRT
                inc     ix
                inc     iy
                pop     bc
                inc     c
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

                ; Handle inline pause countdown
                ld      a,(state.pause_count)
                or      a
                jr      z,.no_pause
                dec     a
                ld      (state.pause_count),a
                jp      .done           ; pausing: no output needed
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
                ; For marquee effects in hold state (marquee_state=1),
                ; dwell expiry triggers scroll-out instead of exit.
                ld      a,(state.fx_display)
                cp      DISP_MARQUEE_L
                jr      z,.dwell_mqchk
                cp      DISP_MARQUEE_R
                jr      z,.dwell_mqchk
                ; Normal: move to exit phase
                ld      a,3
                ld      (state.page_state),a
                xor     a
                ld      (state.fx_pos),a
                ld      (state.fx_sub),a
                jp      .mark_dirty

.dwell_mqchk:   ; Marquee: if still in hold (state 1), start scroll-out
                ld      a,(state.marquee_state)
                cp      1
                jr      nz,.dwell_mqdone
                ; Switch to scroll-out
                ld      a,2
                ld      (state.marquee_state),a
                xor     a
                ld      (state.fx_pos),a
                ; Set tick_count to a large value so dwell doesn't re-expire
                ; while we're scrolling out
                ld      hl,0FFFFh
                ld      (state.tick_count),hl
                jr      .mark_dirty
.dwell_mqdone:  ; Marquee already scrolled out — proceed to exit
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
                jr      nz,.done        ; not time for a step yet

                ; Reload step timer (clamp speed >= 1)
                ld      a,(state.speed)
                or      a
                jr      nz,.spd_ok
                inc     a
.spd_ok:        ld      (state.step_timer),a

                ; Toggle flash phase (once per step, not per tick)
                ld      a,(state.flash_phase)
                xor     1
                ld      (state.flash_phase),a

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
                ; Entry complete → display phase
                ; Don't touch the display buffers — the last fade step already
                ; left them in the correct final state. Just switch state.
                ld      a,2
                ld      (state.page_state),a
                jr      .mark_dirty

.display:       call    fx_display_step
                jr      .mark_dirty

.exit:          call    fx_trans_exit_step
                jr      nc,.mark_dirty
                ; Exit complete — fire callback before loading next page
                call    segfx_callback
                ; If callback returned A != 0, skip next page
                or      a
                jr      z,.exit_done
                ; Skip: advance stream past next PAGE_END
                call    skip_page
.exit_done:     xor     a
                ld      (state.page_state),a
                call    display_clear
                ; fall through

.mark_dirty:    ld      a,1
                ld      (state.dirty),a
.done:          pop     iy
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
; SEGFX_SET_CALLBACK — Register a user callback
; ============================================================================
; Input: HL = callback address (0 to disable)
; Callback contract:
;   Called after each page's exit transition completes.
;   Input:  A = page index (1-based: page 1 just finished)
;   Output: A = 0 to continue normally
;           A != 0 to skip the next page
;   May clobber: AF, BC, DE, HL (engine preserves IX/IY)
; ============================================================================
segfx_set_callback:
                ld      (state.callback),hl
                ret


; ============================================================================
; SEGFX_CALLBACK (internal) — Fire user callback if registered
; ============================================================================
; Returns: A = callback result (0 = continue, nonzero = skip next page)
; ============================================================================
segfx_callback:
                ld      hl,(state.callback)
                ld      a,h
                or      l
                ret     z               ; no callback — A=0 (continue)
                ; Call user function with A = page_index
                ld      a,(state.page_index)
                jp      (hl)            ; tail call — user routine returns to our caller


; ============================================================================
; SKIP_PAGE (internal) — Advance stream_ptr past the next page
; ============================================================================
; Scans forward for PAGE_END or END_STREAM. If END_STREAM, wraps.
; ============================================================================
skip_page:
                ld      hl,(state.stream_ptr)
.sk_lp:         ld      a,(hl)
                cp      END_STREAM
                jr      z,.sk_wrap
                inc     hl
                cp      PAGE_END
                jr      nz,.sk_lp
                ; HL now points past PAGE_END
                ld      (state.stream_ptr),hl
                ret
.sk_wrap:       ld      hl,(state.stream_base)
                ld      (state.stream_ptr),hl
                ret


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
                xor     a
                ld      (state.page_index),a
                jr      .find

.found:         ; Increment page index
                ld      a,(state.page_index)
                inc     a
                ld      (state.page_index),a

                inc     hl              ; skip PAGE_START
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
                ld      e,(hl)          ; dwell_lo
                inc     hl
                ld      d,(hl)          ; dwell_hi
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
                ld      (state.wave_tbl),a
                ld      (state.wave_dir),a
                ld      (custom_count),a
                ld      a,0FEh
                ld      (state.glyph_code),a
                ld      a,0FFh
                ld      (state.inl_bright),a

                call    decode_content
                ld      (state.stream_ptr),hl

                ; For scroll/bounce effects, set initial text_offset so text
                ; doesn't immediately start scrolling off column 0.
                ; Uses align_mode: centre = start half-display in,
                ; right = start at right edge, left = start at column 0 (default).
                ld      a,(state.fx_display)
                cp      DISP_SCROLL_L
                jr      z,.init_scroll_offset
                cp      DISP_SCROLL_R
                jr      z,.init_scroll_offset
                jr      .no_scroll_offset

.init_scroll_offset:
                ; For scroll effects, align_mode means initial blank columns:
                ;   left (0) = no offset, start at column 0
                ;   centre (1) = start with DISP_COLS/2 blank columns
                ;   right (2) = start with DISP_COLS blank columns (full screen of blanks)
                ld      a,(state.align_mode)
                or      a
                jr      z,.no_scroll_offset
                cp      1
                jr      z,.scroll_half
                ; Right: full display of blanks
                ld      b,DISP_COLS
                jr      .scroll_set
.scroll_half:   ld      b,DISP_COLS / 2
.scroll_set:    ; text_offset = wrap_point - B
                ld      a,(state.text_len)
                add     a,DISP_COLS
                sub     b
                ld      (state.text_offset),a
                ; Reset align_mode so render_full/set_full_bright use left-align
                xor     a
                ld      (state.align_mode),a

.no_scroll_offset:

                call    display_clear

                ; TRANS_NONE → skip entry, go straight to display
                ld      a,(state.fx_entry)
                or      a
                jr      nz,.has_entry

                ; Check if display effect builds up from blank
                ; (typewriter, typewriter_r, marquee_l, marquee_r)
                ; These effects render their own content progressively.
                ld      a,(state.fx_display)
                cp      DISP_TYPEWRITER
                jr      z,.blank_start
                cp      DISP_TWRT_R
                jr      z,.blank_start
                cp      DISP_MARQUEE_L
                jr      z,.blank_start
                cp      DISP_MARQUEE_R
                jr      z,.blank_start

                call    render_full
                call    set_full_bright
                ld      a,2
                ld      (state.page_state),a
                ret

.blank_start:   ; Start with blank display; effect will build content
                ld      a,2
                ld      (state.page_state),a
                ret

.has_entry:     ; Pre-render segments so transition only needs to adjust brightness.
                ; display_clear already zeroed brightness, so all columns are dark.
                call    render_full

                ; For vfade IN transitions, save segments to shadow and clear disp_seg
                ld      a,(state.fx_entry)
                cp      TRANS_VFADE_D
                jr      z,.save_shadow
                cp      TRANS_VFADE_U
                jr      z,.save_shadow
                jr      .no_shadow

.save_shadow:   ; Copy disp_seg → shadow_seg
                ld      hl,disp_seg
                ld      de,shadow_seg
                ld      bc,DISP_COLS * 2
                ldir
                ; Clear disp_seg (start with blank segments)
                ld      hl,disp_seg
                ld      b,DISP_COLS * 2
                xor     a
.shclr:         ld      (hl),a
                inc     hl
                djnz    .shclr
                ; Set full brightness so segments are visible as they're revealed
                call    set_full_bright

.no_shadow:     ld      a,1
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
                cp      INL_WAVE_TBL
                jp      z,.i_wtbl
                cp      INL_GLYPH
                jp      z,.i_glyph
                cp      INL_CHARDEF
                jp      z,.i_chardef
                cp      INL_WAVE_DIR
                jp      z,.i_wdir
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
                inc     a               ; 1..16 = new speed value
                ld      (de),a          ; store speed value in text_buf
                ld      (ix+0),0        ; no brightness (invisible)
                ld      (iy+0),1 << ATTR_F_SPEED
                inc     de
                inc     ix
                inc     iy
                inc     c               ; total count, NOT visible count
                ; Also set initial speed from first INL_SPEED encountered
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

.i_wtbl:        inc     hl
                ld      a,(hl)
                sub     INL_PARAM_BASE  ; 0..15
                ld      (state.wave_tbl),a
                inc     hl
                jp      .lp

.i_wdir:        inc     hl
                ld      a,(hl)
                sub     INL_PARAM_BASE  ; 0=L→R, 1=R→L, 2=bounce
                ld      (state.wave_dir),a
                inc     hl
                jp      .lp

.i_glyph:       ; Next 2 bytes = raw segment bitmask
                ; Auto-assign a code, add to custom_chars, emit code as visible char
                ld      a,(custom_count)
                cp      MAX_CUSTOM_CHARS
                jr      nc,.i_gl_skip   ; table full
                ; Save all decode state
                push    de              ; text_buf ptr
                push    bc              ; B=vis_len, C=text_len
                push    ix              ; attr_bright ptr
                push    iy              ; attr_flags ptr
                ; Get auto code
                ld      a,(state.glyph_code)
                ld      c,a             ; C = char code
                dec     a
                ld      (state.glyph_code),a
                ; Read seg pattern from stream
                inc     hl
                ld      e,(hl)          ; seg low
                inc     hl
                ld      d,(hl)          ; seg high
                push    hl              ; save stream ptr (past seg_hi)
                ; Find table slot
                ld      a,(custom_count)
                ld      b,a
                add     a,a             ; ×2
                add     a,b             ; ×3
                ld      l,a
                ld      h,0
                ld      b,0             ; BC = 00:char_code (B cleared)
                push    bc              ; save char code
                ld      bc,custom_chars
                add     hl,bc           ; HL = slot
                pop     bc              ; C = char code
                ; Write entry: code, seg_lo, seg_hi
                ld      (hl),c
                inc     hl
                ld      (hl),e
                inc     hl
                ld      (hl),d
                ; Increment custom count
                ld      a,(custom_count)
                inc     a
                ld      (custom_count),a
                ; Restore stream ptr and decode state
                pop     hl              ; stream ptr
                pop     iy
                pop     ix
                pop     bc              ; B=vis_len, C=text_len
                pop     de              ; text_buf ptr
                ; Emit the auto-assigned code as a visible character
                ld      a,(state.glyph_code)
                inc     a               ; we decremented earlier, get back the code we used
                ld      (de),a          ; text_buf[n] = char code
                ld      a,(state.inl_bright)
                ld      (ix+0),a        ; brightness: normal
                ld      a,(state.inl_flash)
                ld      (iy+0),a        ; flash flag: normal
                inc     de
                inc     ix
                inc     iy
                inc     c               ; total count
                inc     b               ; visible count
                inc     hl
                jp      .lp

.i_gl_skip:     ; Table full — skip 2 bytes
                inc     hl
                inc     hl
                inc     hl
                jp      .lp

.i_chardef:     ; Define a custom character: next byte = code (A0-FE), then 2 bytes = pattern
                ; Stored in custom_chars table for font_lookup to find
                ld      a,(custom_count)
                cp      MAX_CUSTOM_CHARS
                jr      nc,.i_cd_skip   ; table full, ignore
                ; Calculate table slot: custom_chars + count × 3
                push    de
                push    bc
                ld      c,a             ; C = count
                add     a,a             ; ×2
                add     a,c             ; ×3
                ld      e,a
                ld      d,0
                push    hl              ; save stream ptr
                ld      hl,custom_chars
                add     hl,de           ; HL = slot address
                pop     de              ; DE = stream ptr (was HL)
                ; Read: code, seg_lo, seg_hi from stream
                inc     de              ; skip INL_CHARDEF
                ld      a,(de)          ; char code
                ld      (hl),a
                inc     hl
                inc     de
                ld      a,(de)          ; seg low
                ld      (hl),a
                inc     hl
                inc     de
                ld      a,(de)          ; seg high
                ld      (hl),a
                ex      de,hl           ; HL = stream ptr (past last byte)
                ; Increment count
                ld      a,(custom_count)
                inc     a
                ld      (custom_count),a
                pop     bc
                pop     de
                inc     hl
                jp      .lp

.i_cd_skip:     ; Skip the 3 parameter bytes
                inc     hl
                inc     hl
                inc     hl
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

                bit     ATTR_F_SPEED,(iy+0)
                jr      nz,.skip        ; speed change: no render, no column advance

                ; Segment lookup (handles both normal chars and direct glyphs)
                push    bc
                push    ix
                push    iy
                call    char_to_segments ; HL = bitmask
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

                ; Store flash flag at disp_flash[C]
                push    de
                ld      e,c
                ld      d,0
                ld      hl,disp_flash
                add     hl,de
                bit     ATTR_F_FLASH,(iy+0)
                jr      z,.no_fl
                ld      (hl),1
                jr      .fl_done
.no_fl:         ld      (hl),0
.fl_done:       pop     de

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

                bit     ATTR_F_SPEED,(ix+0)
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

                ; Check pause/speed markers
                push    bc
                push    de
                ld      d,0
                ld      hl,attr_flags
                add     hl,de
                bit     ATTR_F_PAUSE,(hl)
                jr      nz,.blank_pop   ; treat pauses as blank
                bit     ATTR_F_SPEED,(hl)
                jr      nz,.blank_pop   ; treat speed changes as blank

                ; Write disp_flash[C] from attr_flags flash bit
                push    hl              ; save attr_flags ptr
                ld      a,0
                bit     ATTR_F_FLASH,(hl)
                jr      z,.scfl0
                inc     a
.scfl0:         ld      hl,disp_flash
                push    de
                ld      e,c
                ld      d,0
                add     hl,de
                pop     de
                ld      (hl),a
                pop     hl              ; HL = attr_flags ptr

                ; Font lookup (handles both standard ASCII and custom chars >= 0xA0)
                ld      hl,text_buf
                push    de
                ld      d,0
                add     hl,de
                pop     de
                ld      a,(hl)
                call    font_lookup     ; HL = bitmask

                pop     de              ; E = source index, D = wrap point
                pop     bc              ; C = display column, B = loop counter

                ; Store at disp_seg[C*2] — must preserve D (wrap point)
                push    de              ; save D (wrap point)
                push    hl              ; save bitmask
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
                pop     de              ; restore D = wrap point
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
                cp      TRANS_VFADE_D
                jp      z,fx_vfade_d_in
                cp      TRANS_VFADE_U
                jp      z,fx_vfade_u_in
                scf                     ; unknown → done
                ret

; --- FADE IN: 15 steps, brightness = step × 17 (17, 34, ... 255) ---
fx_fade_in:
                ld      a,(state.fx_pos)
                cp      15
                jr      nc,.done
                inc     a
                ld      (state.fx_pos),a
                ; brightness = step × 17 (step 1..15 → 17..255)
                ld      b,a
                sla     a
                sla     a
                sla     a
                sla     a               ; ×16
                add     a,b             ; ×17
                jr      nc,.ok
                ld      a,0FFh          ; clamp (shouldn't happen for steps 1..15)
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
;               call    render_full  ; removed: pre-rendered in page_load
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
;               call    render_full  ; removed: pre-rendered in page_load
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
;               call    render_full  ; removed: pre-rendered in page_load
                ld      a,(state.fx_pos)
                cp      DISP_COLS / 2
                jr      nc,.done
                ld      b,a
                ld      a,DISP_COLS / 2 - 1
                sub     b
                call    bright_col_ff
                ld      a,DISP_COLS / 2
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
;               call    render_full  ; removed: pre-rendered in page_load
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
;               call    render_full  ; removed: pre-rendered in page_load
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
;               call    render_full  ; removed: pre-rendered in page_load
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
;               call    render_full  ; removed: pre-rendered in page_load
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
                cp      TRANS_VFADE_D
                jp      z,fx_vfade_d_out
                cp      TRANS_VFADE_U
                jp      z,fx_vfade_u_out
                scf
                ret

; --- FADE OUT: 16 steps, brightness = (15-pos) × 17 (255, 238, ... 17, 0) ---
fx_fade_out:
                ld      a,(state.fx_pos)
                cp      16
                jr      nc,.done
                ; brightness = (15 - pos) × 17 (step 0..15 → 255..0)
                ld      b,a
                ld      a,15
                sub     b               ; 15..0
                ld      c,a
                sla     a
                sla     a
                sla     a
                sla     a               ; ×16
                add     a,c             ; ×17
                ld      hl,disp_bright
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
                ld      a,DISP_COLS / 2 - 1
                sub     b
                call    clear_col
                ld      a,DISP_COLS / 2
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


; --- VERTICAL FADE DOWN IN: reveal rows top-to-bottom ---
; Each step adds a row. Reads from shadow_seg, writes masked result to disp_seg.
fx_vfade_d_in:
                ld      a,(state.fx_pos)
                cp      5
                jr      nc,.done
                ; Build cumulative mask: OR rows 0..fx_pos
                ld      c,a
                inc     c               ; C = number of rows to include (1..5)
                ld      hl,vfade_row_masks
                ld      de,0            ; DE = cumulative mask
.cmask:         ld      a,e
                or      (hl)
                ld      e,a
                inc     hl
                ld      a,d
                or      (hl)
                ld      d,a
                inc     hl
                dec     c
                jr      nz,.cmask
                ; DE = cumulative mask. Apply: disp_seg[i] = shadow_seg[i] AND mask
                ld      hl,shadow_seg
                ld      ix,disp_seg
                ld      b,DISP_COLS
.lp:            ld      a,(hl)
                and     e
                ld      (ix+0),a
                inc     hl
                ld      a,(hl)
                and     d
                ld      (ix+1),a
                inc     hl
                inc     ix
                inc     ix
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- VERTICAL FADE UP IN: reveal rows bottom-to-top ---
fx_vfade_u_in:
                ld      a,(state.fx_pos)
                cp      5
                jr      nc,.done
                ; Build cumulative mask: OR rows 4, 3, 2... (4-fx_pos rows from bottom)
                ld      c,a
                inc     c               ; C = number of rows to include
                ; Start row index = 4 - fx_pos
                ld      b,a
                ld      a,4
                sub     b               ; A = start row index
                ; Point HL to vfade_row_masks[start_row]
                add     a,a             ; ×2
                ld      e,a
                ld      d,0
                ld      hl,vfade_row_masks
                add     hl,de
                ; Now OR C consecutive rows going forward
                ld      de,0
.cmask:         ld      a,e
                or      (hl)
                ld      e,a
                inc     hl
                ld      a,d
                or      (hl)
                ld      d,a
                inc     hl
                dec     c
                jr      nz,.cmask
                ; Apply
                ld      hl,shadow_seg
                ld      ix,disp_seg
                ld      b,DISP_COLS
.lp:            ld      a,(hl)
                and     e
                ld      (ix+0),a
                inc     hl
                ld      a,(hl)
                and     d
                ld      (ix+1),a
                inc     hl
                inc     ix
                inc     ix
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- VERTICAL FADE DOWN OUT: clear rows top-to-bottom ---
fx_vfade_d_out:
                ld      a,(state.fx_pos)
                cp      5
                jr      nc,.done
                ; Get inverse mask for row fx_pos (top-down: 0,1,2,3,4)
                ld      e,a
                ld      d,0
                ld      hl,vfade_row_masks
                add     hl,de
                add     hl,de
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                ; Invert: AND mask = NOT(row mask)
                ld      a,e
                cpl
                ld      e,a
                ld      a,d
                cpl
                ld      d,a             ; DE = ~row_mask
                ld      hl,disp_seg
                ld      b,DISP_COLS
.lp:            ld      a,(hl)
                and     e
                ld      (hl),a
                inc     hl
                ld      a,(hl)
                and     d
                ld      (hl),a
                inc     hl
                djnz    .lp
                ld      hl,state.fx_pos
                inc     (hl)
                or      a
                ret
.done:          scf
                ret

; --- VERTICAL FADE UP OUT: clear rows bottom-to-top ---
fx_vfade_u_out:
                ld      a,(state.fx_pos)
                cp      5
                jr      nc,.done
                ld      b,a
                ld      a,4
                sub     b               ; A = 4-fx_pos
                ld      e,a
                ld      d,0
                ld      hl,vfade_row_masks
                add     hl,de
                add     hl,de
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                ld      a,e
                cpl
                ld      e,a
                ld      a,d
                cpl
                ld      d,a
                ld      hl,disp_seg
                ld      b,DISP_COLS
.lp:            ld      a,(hl)
                and     e
                ld      (hl),a
                inc     hl
                ld      a,(hl)
                and     d
                ld      (hl),a
                inc     hl
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
                ld      hl,wave_tbl_sine
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
                call    check_offset_speed
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
                call    check_offset_speed
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
                jr      z,.tw_chk_spd

                ; Load pause duration and advance
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                ld      (state.pause_count),a
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.tw_chk_spd:    ; Speed change marker?
                bit     ATTR_F_SPEED,(hl)
                jr      z,.tw_vis

                ; Apply new speed and advance (no visible output)
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                ld      (state.speed),a
                ld      (state.step_timer),a
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
                ; Font lookup (handles standard + custom chars)
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
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
                bit     ATTR_F_SPEED,(hl)
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
                ret     c               ; fits on display, nothing to do

                ; Render at current offset first
                call    render_scrolled
                call    set_all_bright

                ; Then adjust offset for next step
                ld      a,(state.bounce_dir)
                or      a
                jr      nz,.right

                ; Moving left: increment offset
                ld      a,(state.text_offset)
                ld      b,a
                ld      a,(state.text_len)
                sub     DISP_COLS
                cp      b
                jr      z,.flip_right   ; at end, reverse
                ld      a,b
                inc     a
                ld      (state.text_offset),a
                jp      check_offset_speed

.flip_right:    ld      a,1
                ld      (state.bounce_dir),a
                ret

.right:         ; Moving right: decrement offset
                ld      a,(state.text_offset)
                or      a
                jr      z,.flip_left    ; at start, reverse
                dec     a
                ld      (state.text_offset),a
                jp      check_offset_speed

.flip_left:     xor     a
                ld      (state.bounce_dir),a
                ret


; ============================================================================
; Helper: check_offset_speed — if text_buf[text_offset] is a speed marker,
; apply it. Call after updating text_offset.
; ============================================================================
check_offset_speed:
                ld      a,(state.text_offset)
                ld      e,a
                ld      d,0
                ld      a,(state.text_len)
                cp      e
                ret     z               ; in gap region
                ret     c               ; in gap region
                ld      hl,attr_flags
                add     hl,de
                bit     ATTR_F_SPEED,(hl)
                ret     z               ; not a speed entry
                ld      hl,text_buf
                add     hl,de
                ld      a,(hl)
                ld      (state.speed),a
                ld      (state.step_timer),a
                ret


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
                ; Copy remaining dwell ticks as hold duration
                ld      hl,(state.tick_count)
                ld      (state.marquee_hold),hl
                ; Set tick_count high so normal dwell doesn't expire during hold/scroll-out
                ld      hl,0FFFFh
                ld      (state.tick_count),hl
                ; Render in final centered position
                call    render_full
                call    set_full_bright
                ret

.mql_hold:      ; Count down hold timer every step
                ; Subtract speed from hold each step (approximates per-tick)
                ld      hl,(state.marquee_hold)
                ld      a,(state.speed)
                ld      c,a
                ld      b,0
                or      a               ; clear carry
                sbc     hl,bc           ; hl -= speed
                jr      c,.mql_hold_done
                ld      a,h
                or      l
                jr      z,.mql_hold_done
                ld      (state.marquee_hold),hl
                ret
.mql_hold_done: ; Hold expired — start scroll out
                ld      a,2
                ld      (state.marquee_state),a
                xor     a
                ld      (state.fx_pos),a
                ret

.mql_out:       ; Scroll out left: text slides from centered to off-screen left
                ; Done when start_col - fx_pos + vis_len <= 0
                ; i.e. fx_pos >= calc_start_col + vis_len
                ld      a,(state.fx_pos)
                ld      b,a             ; B = fx_pos
                push    bc
                call    calc_start_col  ; A = start_col
                pop     bc
                ld      c,a             ; C = start_col
                ld      a,(state.vis_len)
                add     a,c             ; A = start_col + vis_len = rightmost col + 1
                cp      b
                jr      z,.mql_gone     ; fx_pos == start_col + vis_len: fully off
                jr      c,.mql_gone     ; fx_pos > start_col + vis_len: fully off

                ; Render at column (start_col - fx_pos); may be negative = partially off
                ld      a,c
                sub     b               ; A = start_col - fx_pos (signed)
                call    .mql_render_at
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.mql_gone:      ; Off screen — force dwell to expire so we move on immediately
                call    display_clear
                xor     a
                ld      (state.tick_count),a
                ld      (state.tick_count+1),a
                ret

; Helper: render text starting at display column A (may be negative/underflowed)
; Clears display first, then places glyphs starting at column A
; If A > 127 (i.e. underflowed from negative), skip leading chars until on-screen
.mql_render_at:
                ld      c,a             ; C = start column (may be underflowed)
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

.mrl:           ; Check if column is off the right edge
                ld      a,c
                cp      DISP_COLS
                jr      nc,.mrl_offscr  ; C >= 24: either off right or underflowed negative

                bit     ATTR_F_PAUSE,(iy+0)
                jr      nz,.mrs         ; skip pause, no column advance
                bit     ATTR_F_SPEED,(iy+0)
                jr      nz,.mrs         ; skip speed, no column advance

                push    bc
                push    ix
                push    iy
                call    char_to_segments
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
                jr      .mrs

.mrl_offscr:    ; C >= 24. If C > 127, it's an underflowed negative — skip char, advance C
                ; If C < 128 (i.e. 24..127), we're past right edge — done
                bit     7,c
                ret     z               ; C is 24..127 = past right edge, done

                ; Negative column: skip this char but still advance column
                bit     ATTR_F_PAUSE,(iy+0)
                jr      nz,.mrs         ; pause: no column advance
                bit     ATTR_F_SPEED,(iy+0)
                jr      nz,.mrs         ; speed: no column advance
                inc     c               ; advance column (towards 0)

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

.mqr_in:        ; Text slides in from left: starts off-screen, moving right
                ; Start column = fx_pos - vis_len (negative while entering)
                ; mql_render_at handles negative columns: skips chars until on-screen
                ld      a,(state.fx_pos)
                ld      d,a             ; D = fx_pos
                push    de
                call    calc_start_col
                pop     de
                ld      e,a             ; E = target (centered column)
                ; Compute start_col = fx_pos - vis_len
                ld      a,(state.vis_len)
                ld      c,a             ; C = vis_len
                ld      a,d             ; fx_pos
                sub     c               ; A = fx_pos - vis_len (may underflow, that's fine)
                ld      b,a             ; B = start_col for render
                ; Check if centered: need fx_pos >= vis_len AND (fx_pos - vis_len) >= target
                ld      a,d             ; fx_pos
                cp      c               ; vis_len
                jr      c,.mqr_render   ; fx_pos < vis_len: still entering, not centered
                ; fx_pos >= vis_len: start_col is valid (non-negative)
                ld      a,b             ; start_col
                cp      e               ; target
                jr      nc,.mqr_centered ; start_col >= target: reached center
.mqr_render:
                ld      a,b             ; start column (underflowed = negative, handled by render_at)
                call    fx_dsp_marquee_l.mql_render_at
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.mqr_centered:
                ld      a,1
                ld      (state.marquee_state),a
                ld      hl,(state.tick_count)
                ld      (state.marquee_hold),hl
                ld      hl,0FFFFh
                ld      (state.tick_count),hl
                call    render_full
                call    set_full_bright
                ret

.mqr_hold:      ld      hl,(state.marquee_hold)
                ld      a,(state.speed)
                ld      c,a
                ld      b,0
                or      a
                sbc     hl,bc
                jr      c,.mqr_hold_done
                ld      a,h
                or      l
                jr      z,.mqr_hold_done
                ld      (state.marquee_hold),hl
                ret
.mqr_hold_done: ld      a,2
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
                xor     a
                ld      (state.tick_count),a
                ld      (state.tick_count+1),a
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
                jr      z,.twr_chk_spd

                ; Pause
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                ld      (state.pause_count),a
                ld      hl,state.fx_pos
                inc     (hl)
                ret

.twr_chk_spd:  ; Speed change marker?
                bit     ATTR_F_SPEED,(hl)
                jr      z,.twr_vis

                ; Apply new speed and advance
                ld      hl,text_buf
                add     hl,bc
                ld      a,(hl)
                ld      (state.speed),a
                ld      (state.step_timer),a
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
                ; Compute table base: wave_tables + (wave_tbl × 32)
                ld      a,(state.wave_tbl)
                ; Clamp to valid range
                cp      WAVE_TBL_COUNT
                jr      c,.tbl_ok
                xor     a               ; default to sine
.tbl_ok:        ld      e,a
                ld      d,0
                ; ×32: shift left 5
                sla     e
                rl      d
                sla     e
                rl      d
                sla     e
                rl      d
                sla     e
                rl      d
                sla     e
                rl      d               ; DE = wave_tbl × 32
                ld      ix,wave_tables
                add     ix,de           ; IX = table base

                ld      a,(state.fx_pos)
                ld      d,a             ; D = phase offset
                ld      hl,disp_bright
                ld      b,DISP_COLS
                ld      c,0
.lp:            ld      a,d
                add     a,c
                and     1Fh             ; mod 32
                push    hl
                push    bc
                push    de
                ld      e,a
                ld      d,0
                push    ix
                pop     hl              ; HL = table base
                add     hl,de
                ld      a,(hl)
                pop     de
                pop     bc
                pop     hl
                ld      (hl),a
                inc     hl
                inc     c
                djnz    .lp
                ; Update fx_pos based on wave direction
                ld      a,(state.wave_dir)
                or      a
                jr      z,.dir_ltr      ; 0 = L→R
                dec     a
                jr      z,.dir_rtl      ; 1 = R→L
                ; 2 = bounce — use fx_sub as sub-direction (0=L→R, 1=R→L)
                ld      a,(state.fx_sub)
                or      a
                jr      nz,.dir_rtl_b
                ; bounce L→R: decrement
                ld      hl,state.fx_pos
                dec     (hl)
                ld      a,(hl)
                and     1Fh
                ret     nz
                ; wrapped — flip to R→L
                ld      a,1
                ld      (state.fx_sub),a
                ret
.dir_rtl_b:     ; bounce R→L: increment
                ld      hl,state.fx_pos
                inc     (hl)
                ld      a,(hl)
                and     1Fh
                ret     nz
                ; wrapped — flip to L→R
                xor     a
                ld      (state.fx_sub),a
                ret
.dir_ltr:       ld      hl,state.fx_pos
                dec     (hl)
                ret
.dir_rtl:       ld      hl,state.fx_pos
                inc     (hl)
                ret



; ============================================================================
; DEMO MESSAGE STREAM
; ============================================================================
example_stream:
                ; Page 1: Title — fade in, breathe, fade out
                db      PAGE_START
                db      TRANS_FADE
                db      DISP_BREATHE
                db      TRANS_FADE
                db      2
                dw      300             ; 300 ticks = 6 sec
                db      "  SPACE INVADERS 3D   "
                db      PAGE_END

                ; Page 2: Scrolling instructions (with speed changes)
                ; INL_ALIGN_C gives ~12 columns of leading blank before scroll starts
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_SCROLL_L
                db      TRANS_NONE
                db      2               ; initial speed: fast
                dw      800             ; 800 ticks = 16 sec
                db      INL_ALIGN_C
                db      "ARROWS TO MOVE"
                db      INL_SPEED, INL_PARAM_BASE + 4
                db      " --- "
                db      INL_SPEED, INL_PARAM_BASE + 1
                db      "Z TO FIRE"
                db      INL_SPEED, INL_PARAM_BASE + 4
                db      " --- "
                db      INL_SPEED, INL_PARAM_BASE + 1
                db      "P TO PAUSE"
                db      INL_SPEED, INL_PARAM_BASE + 4
                db      " --- "
                db      INL_SPEED, INL_PARAM_BASE + 1
                db      "SURVIVE AS LONG AS YOU CAN!"
                db      PAGE_END

                ; Page 3: Marquee left
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_MARQUEE_L
                db      TRANS_NONE
                db      2
                dw      100             ; 100 ticks = 2 sec hold
                db      INL_ALIGN_C
                db      "WAVE 1"
                db      PAGE_END

                ; Page 4: Typewriter with acceleration
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_TYPEWRITER
                db      TRANS_FADE
                db      8               ; initial speed: slow
                dw      400             ; 400 ticks = 8 sec
                db      INL_ALIGN_C
                db      INL_SPEED, INL_PARAM_BASE + 7
                db      "GET "
                db      INL_SPEED, INL_PARAM_BASE + 1
                db      "READY"
                db      INL_PAUSE, INL_PARAM_BASE + 10
                db      INL_SPEED, INL_PARAM_BASE + 3
                db      "..."
                db      PAGE_END

                ; Page 5: Sparkle with vfade-up in, fade-wipe out
                db      PAGE_START
                db      TRANS_VFADE_U
                db      DISP_SPARKLE
                db      TRANS_FADE_WP_R
                db      2
                dw      500             ; 500 ticks = 10 sec
                db      INL_ALIGN_C
                db      "HIGH SCORE  12500"
                db      PAGE_END

                ; Page 6: Glint highlight sweep
                db      PAGE_START
                db      TRANS_VFADE_D
                db      DISP_WAVE
                db      TRANS_VFADE_U
                db      1               ; fast speed for smooth glint
                dw      300             ; 300 ticks = 6 sec
                db      INL_WAVE_TBL, INL_PARAM_BASE + 1   ; table 1 = glint
                db      INL_WAVE_DIR, INL_PARAM_BASE + 2   ; bounce
                db      "    ROUND COMPLETE!     "
                db      PAGE_END

                ; Page 7: Reverse typewriter
                db      PAGE_START
                db      TRANS_NONE
                db      DISP_TWRT_R
                db      TRANS_WIPE_EDGE
                db      3
                dw      300             ; 300 ticks = 6 sec
                db      INL_ALIGN_C
                db      "GAME OVER"
                db      PAGE_END

                ; Page 8: Bouncing credits (accelerates at edges)
                db      PAGE_START
                db      TRANS_WIPE_L
                db      DISP_BOUNCE
                db      TRANS_WIPE_R
                db      4               ; initial speed: moderate
                dw      1000            ; 1000 ticks = 20 sec
                db      "PROGRAMMED BY"
                db      INL_SPEED, INL_PARAM_BASE + 1
                db      " TEAM ALPHA"
                db      INL_SPEED, INL_PARAM_BASE + 3
                db      " --- 2026"
                db      PAGE_END

                ; Page 9: Per-char flash — "INSERT" solid, "COIN" flashes
                db      PAGE_START
                db      TRANS_WIPE_CTR
                db      DISP_STATIC
                db      TRANS_WIPE_EDGE
                db      6
                dw      600             ; 600 ticks = 12 sec
                db      INL_ALIGN_C
                db      "INSERT "
                db      INL_FLASH
                db      "COIN"
                db      PAGE_END

                ; Page 10: Custom characters demo
                ; Heart: h,j (upper bumps), g1,g2 (middle), m,k (lower V)
                ;   bits: h=8, j=10, g1=6, g2=7, k=11, m=13 = 0x2DC0
                ; Right arrow: a,g1,g2,d,j,k = pointing right
                ;   bits: a=0, d=3, g1=6, g2=7, j=10, k=11 = 0x0CC9
                db      PAGE_START
                db      TRANS_FADE
                db      DISP_STATIC
                db      TRANS_FADE
                db      3
                dw      600             ; 600 ticks = 12 sec
                db      INL_CHARDEF, 0A0h, 001h, 007h  ; heart = 0x0701 (lo, hi)
                db      INL_ALIGN_C
                db      "I "
                db      0A0h            ; heart character
                db      " Z80 "
                db      INL_GLYPH, 040h, 021h          ; right arrow = 0x2140 (lo, hi)
                db      PAGE_END

                db      END_STREAM
