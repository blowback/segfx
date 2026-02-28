# SEGFX — 14-Segment Display Effect Engine

## Protocol Specification v1.0


https://github.com/user-attachments/assets/4093a997-58f3-42f5-ad0f-ccc998fab88e


**Target:** Z80 microcomputer at 8 MHz with 24× 14-segment character displays
**Display:** Single row, 24 characters, 8-bit (256-level) per-character brightness via hardware PWM
**Interface:** BIOS calls for segment patterns and brightness
**Timing:** 50 Hz tick (20ms per tick)

---

## 1. Architecture Overview

### Concept

A **message stream** is a sequence of **pages**. Each page contains text content with optional inline effect codes. Pages play one after another, each with its own entry transition, display effect, dwell time, and exit transition. When `END_STREAM` is reached, the stream loops from the beginning.

```
Stream = [Page₀] [Page₁] [Page₂] ... [END_STREAM → loop]
```

### Page Lifecycle

```
  ┌─ page_load ─┐
  │              ▼
  │    ┌─ Entry Transition ─┐
  │    │  (page_state = 1)  │
  │    └────────┬───────────┘
  │             ▼
  │    ┌─ Display Effect ───┐
  │    │  (page_state = 2)  │  ← dwell timer counts down
  │    └────────┬───────────┘
  │             ▼
  │    ┌─ Exit Transition ──┐
  │    │  (page_state = 3)  │
  │    └────────┬───────────┘
  │             ▼
  └──── next page ──────────┘
```

### Display Buffers

| Buffer | Size | Purpose |
|--------|------|---------|
| `disp_seg[24]` | 48 bytes | Segment bitmasks (2 bytes per column) |
| `disp_bright[24]` | 24 bytes | Brightness per column (0x00–0xFF) |
| `disp_flash[24]` | 24 bytes | Per-column flash flag (0=solid, 1=flashing) |
| `shadow_seg[24]` | 48 bytes | Shadow copy for segment-masking transitions |
| `text_buf[256]` | 256 bytes | Decoded text for current page |
| `attr_bright[256]` | 256 bytes | Per-character brightness attribute |
| `attr_flags[256]` | 256 bytes | Per-character flags (flash, pause, speed) |

### Brightness Scaling

All internal brightness values use the full 0–255 range. At output time, values are scaled to `MAX_BRIGHT` (default 128) via a right-shift. This compresses the usable range to match hardware characteristics where upper brightness levels are indistinguishable. Adjust `MAX_BRIGHT` to suit your display.

### Timing

The engine is driven by `segfx_tick`, called at **50 Hz** from a BIOS interrupt hook.

**Speed** parameters are expressed as **ticks per step** (higher = slower):
- Speed 1 = step every 20ms (50 steps/sec)
- Speed 2 = step every 40ms (25 steps/sec)
- Speed 5 = step every 100ms (10 steps/sec)

**Dwell** is a 16-bit value in ticks (little-endian in stream). 50 ticks = 1 second. Max ~21 minutes.

### Main Loop Integration

```z80
; Install segfx_tick as the 50Hz interrupt handler.
; In your main loop, call segfx_update to push changes to hardware:
main_loop:
    halt                    ; wait for interrupt (segfx_tick runs)
    call    segfx_update    ; push display buffers to hardware if dirty
    jr      main_loop
```

`segfx_update` checks a dirty flag and only calls `segfx_output` when buffers have changed, avoiding unnecessary BIOS calls.

---

## 2. Byte Stream Format

### 2.1 Stream Structure

```
[PAGE] [PAGE] [PAGE] ... 0x00
```

`0x00` = `END_STREAM` — loops back to the first page.

### 2.2 Page Header (7 bytes)

```
0x01            ; PAGE_START marker
<entry_fx>      ; Entry transition code (1 byte)
<display_fx>    ; Display effect code (1 byte)
<exit_fx>       ; Exit transition code (1 byte)
<speed>         ; Ticks per animation step (1 byte, 1–16 typical)
<dwell_lo>      ; Dwell time low byte
<dwell_hi>      ; Dwell time high byte
[CONTENT...]    ; Text and inline codes
0x02            ; PAGE_END marker
```

### 2.3 Content Bytes

| Range | Meaning |
|-------|---------|
| `0x20–0x7E` | Printable ASCII (space through tilde) |
| `0x80–0x8F` | Inline effect codes (see §3.3) |
| `0x90–0x9F` | Inline parameter values (0–15) |
| `0x01` | PAGE_START (page boundary only) |
| `0x02` | PAGE_END |
| `0x00` | END_STREAM |

---

## 3. Effect Reference

### 3.1 Entry/Exit Transitions

The same code is used for both entry and exit. As entry, the transition reveals the page; as exit, it conceals it.

| Code | Name | Steps | Description |
|------|------|-------|-------------|
| `0x00` | `TRANS_NONE` | 0 | Instant appear/disappear. No transition animation. |
| `0x01` | `TRANS_FADE` | 15 in / 16 out | All 24 columns fade brightness up (in) or down (out) simultaneously. Smooth ramp in steps of 17. |
| `0x03` | `TRANS_WIPE_L` | 24 | Columns reveal/conceal one at a time, left to right. |
| `0x04` | `TRANS_WIPE_R` | 24 | Columns reveal/conceal one at a time, right to left. |
| `0x05` | `TRANS_WIPE_CTR` | 12 | Columns reveal/conceal from centre outward, two at a time. |
| `0x06` | `TRANS_WIPE_EDGE` | 12 | Columns reveal/conceal from edges inward, two at a time. |
| `0x07` | `TRANS_DISSOLVE` | 24 | Columns reveal/conceal in pseudo-random order. |
| `0x08` | `TRANS_FADE_WP_L` | 27 | Left-to-right wipe with 3-column fade trail. |
| `0x09` | `TRANS_FADE_WP_R` | 27 | Right-to-left wipe with 3-column fade trail. |
| `0x0A` | `TRANS_VFADE_D` | 5 | Vertical segment fade, top row first. Reveals/conceals segment rows top-to-bottom. |
| `0x0B` | `TRANS_VFADE_U` | 5 | Vertical segment fade, bottom row first. Reveals/conceals segment rows bottom-to-top. |

**Transition duration** = steps × speed ticks × 20ms. For example, `TRANS_FADE` at speed 2 = 16 × 2 × 20ms = 640ms.

#### Vertical Fade Segment Rows

The vertical fade transitions operate on 5 horizontal rows of segments:

| Row | Position | Bitmask |
|-----|----------|---------|
| 0 | Top horizontal (a) | `0x0001` |
| 1 | Upper verticals + diagonals (b, f, h, j, k) | `0x0722` |
| 2 | Mid horizontals (g1, g2) | `0x00C0` |
| 3 | Lower verticals + diagonals (c, e, l, m, n) | `0x3814` |
| 4 | Bottom horizontal + dp (d, dp) | `0x4008` |

As an **in** transition, rows are revealed one at a time (characters "draw" from top-down or bottom-up). As an **out** transition, rows are stripped away.

### 3.2 Display Effects

These run continuously during the dwell period between entry and exit transitions.

| Code | Name | Description | Relevant Inline Codes |
|------|------|-------------|----------------------|
| `0x00` | `DISP_STATIC` | Hold text at current brightness. No animation. Per-char flash (`INL_FLASH`) still active. | `INL_FLASH`, `INL_BRIGHT`, `INL_ALIGN_*` |
| `0x01` | `DISP_FLASH` | All characters flash on/off together. Rate = page speed. | `INL_BRIGHT`, `INL_ALIGN_*` |
| `0x02` | `DISP_BREATHE` | Sinusoidal brightness pulse across all characters. Always uses the sine wave table. | `INL_ALIGN_*` |
| `0x03` | `DISP_SCROLL_L` | Continuous left scroll. Text + 24-column gap wraps as a ring buffer. | `INL_SPEED`, `INL_ALIGN_*` (initial offset) |
| `0x04` | `DISP_SCROLL_R` | Continuous right scroll. | `INL_SPEED`, `INL_ALIGN_*` (initial offset) |
| `0x05` | `DISP_MARQUEE_L` | Scroll in from right, hold centred, scroll out left. | `INL_ALIGN_*` |
| `0x06` | `DISP_MARQUEE_R` | Scroll in from left, hold centred, scroll out right. | `INL_ALIGN_*` |
| `0x07` | `DISP_TYPEWRITER` | Characters revealed left to right. Respects pauses and speed changes. | `INL_PAUSE`, `INL_SPEED`, `INL_FLASH`, `INL_ALIGN_*` |
| `0x08` | `DISP_TWRT_R` | Characters revealed right to left. | `INL_PAUSE`, `INL_SPEED`, `INL_FLASH`, `INL_ALIGN_*` |
| `0x09` | `DISP_BOUNCE` | Text pans left then right, reversing at edges. | `INL_SPEED` |
| `0x0A` | `DISP_FLASH_ALT` | Alternating characters flash (even on / odd off, then swap). | `INL_ALIGN_*` |
| `0x0B` | `DISP_SPARKLE` | Random characters briefly dim and restore (twinkling). | `INL_ALIGN_*` |
| `0x0C` | `DISP_WAVE` | Brightness wave travels across display. Table selectable. | `INL_WAVE_TBL`, `INL_ALIGN_*` |

#### Notes on Specific Effects

**Scroll effects (DISP_SCROLL_L / DISP_SCROLL_R):**
Text wider than 24 characters scrolls through a ring buffer with a 24-column blank gap between repeats. `INL_SPEED` entries in the text take effect as the scroll position passes them. Alignment controls the initial offset before scrolling begins: `INL_ALIGN_L` = no offset (default), `INL_ALIGN_C` = 12 blank leading columns, `INL_ALIGN_R` = 24 blank columns. For `DISP_SCROLL_R`, alignment controls where character 0 initially appears on the display.

**Marquee effects (DISP_MARQUEE_L / DISP_MARQUEE_R):**
Three-phase animation: scroll in → hold → scroll out. The page's **dwell time** controls the hold duration. Best suited for short text that fits on the display; text wider than 24 columns is clipped during hold. After scrolling out, the engine immediately proceeds to the exit transition (no blank time).

**Typewriter effects (DISP_TYPEWRITER / DISP_TWRT_R):**
Characters are revealed one per step. `INL_PAUSE` inserts a delay (in steps). `INL_SPEED` changes the reveal rate mid-text. Both are invisible — they occupy buffer slots but no display columns. Use `TRANS_NONE` as the entry transition; other entries will pre-render the text before the typewriter starts.

**Bounce (DISP_BOUNCE):**
Requires text longer than 24 characters. Renders at the current offset, then adjusts for the next step — both endpoints (offset 0 and max) are displayed. `INL_SPEED` within the text changes pan speed as different sections are shown.

**Wave (DISP_WAVE):**
Uses a 32-entry brightness lookup table, selectable via `INL_WAVE_TBL`. Phase advances one position per step. At speed 1, a full wave cycle = 32 × 20ms = 640ms.

### 3.3 Inline Effect Codes

Inline codes appear within page content. They modify rendering state but produce no visible output.

| Code | Name | Parameter | Description |
|------|------|-----------|-------------|
| `0x80` | `INL_BRIGHT` | `0x90–0x9F` | Set brightness for subsequent characters. 16 levels. |
| `0x81` | `INL_FLASH` | — | Enable per-character flash for subsequent characters. |
| `0x82` | `INL_NOFLASH` | — | Disable per-character flash. |
| `0x83` | `INL_PAUSE` | `0x90–0x9F` | Typewriter pause. Duration = parameter value in steps. |
| `0x84` | `INL_SPEED` | `0x90–0x9F` | Change speed. New speed = parameter + 1 (1–16 ticks/step). |
| `0x85` | `INL_ALIGN_C` | — | Centre-align. For scroll: 12-column initial offset. |
| `0x86` | `INL_ALIGN_R` | — | Right-align. For scroll: 24-column initial offset. |
| `0x87` | `INL_ALIGN_L` | — | Left-align (default). |
| `0x88` | `INL_WAVE_TBL` | `0x90–0x9F` | Select wave table index for DISP_WAVE. |
| `0x89` | `INL_GLYPH` | 2 bytes: seg_lo, seg_hi | Insert a one-off custom glyph. Rendered as a single visible character. |
| `0x8A` | `INL_CHARDEF` | 3 bytes: code, seg_lo, seg_hi | Define a reusable custom character in the 0xA0–0xFE range. |

#### Important Usage Notes

**`INL_SPEED` at text start:** Creates a phantom blank column in scroll/bounce effects. Use the page header speed for the initial value; only use `INL_SPEED` for mid-text changes.

**`INL_FLASH` composability:** Applied at the output stage, not in display buffers. Composes correctly with any display effect — a breathing page with flash characters will have flash chars blink while others breathe.

**`INL_ALIGN_*` dual meaning:** For static/typewriter effects, controls text positioning. For scroll effects, controls initial blank offset. For marquee, controls the centred hold position.

### 3.4 Custom Characters

Two mechanisms for displaying arbitrary segment patterns beyond the built-in font.

#### INL_CHARDEF — Reusable Custom Characters

Defines a character code in the range `0xA0–0xFE` with a custom segment pattern. Once defined, the code can be used anywhere in the page content just like a normal ASCII character.

```
0x8A <code> <seg_lo> <seg_hi>
```

- `code`: character code `0xA0–0xFE`
- `seg_lo`, `seg_hi`: 16-bit segment bitmask (little-endian)

The definition is invisible — it produces no display output. The character code can then appear in content bytes and will be rendered using the defined pattern. Definitions persist for the duration of the page and are cleared on page load.

**Example — define a heart as 0xA0 and use it:**
```
8A A0 01 07     ; INL_CHARDEF: code=0xA0, segments=0x0701
"I "
A0              ; renders the heart glyph
" Z80"
```

#### INL_GLYPH — One-Off Inline Glyphs

Inserts a single custom character directly at the current position. The engine auto-assigns a code internally and stores it in the same custom character table.

```
0x89 <seg_lo> <seg_hi>
```

The glyph is a visible character — it occupies one display column. All inline attributes (`INL_BRIGHT`, `INL_FLASH`, etc.) apply to it normally.

**Example — inline right arrow:**
```
89 40 21        ; INL_GLYPH: segments=0x2140 (right arrow)
```

#### Limits and Behaviour

Both `INL_CHARDEF` and `INL_GLYPH` share a single table of **16 entries** per page (`MAX_CUSTOM_CHARS`). `INL_GLYPH` auto-assigns codes counting down from `0xFE`. If the table is full, additional definitions/glyphs are silently ignored.

Custom characters work with all display effects and transitions — they go through the same `font_lookup` path as standard characters. All per-character attributes (brightness, flash) apply normally.

#### Segment Bit Mapping

The segment bitmask uses this bit assignment (from datasheet):

```
Bit  0 = a   (top horizontal)            = 1
Bit  1 = b   (upper right vertical)      = 2
Bit  2 = c   (lower right vertical)      = 4
Bit  3 = d   (bottom horizontal)         = 8
Bit  4 = e   (lower left vertical)       = 16
Bit  5 = f   (upper left vertical)       = 32
Bit  6 = g1  (mid left horizontal)       = 64
Bit  7 = g2  (mid right horizontal)      = 128
Bit  8 = h   (top-left diagonal ↘)       = 256
Bit  9 = j   (upper-mid vertical ↓)      = 512
Bit 10 = k   (top-right diagonal ↙)      = 1024
Bit 11 = l   (bottom-right diagonal ↗)   = 2048
Bit 12 = m   (lower-mid vertical ↑)      = 4096
Bit 13 = n   (bottom-left diagonal ↘)    = 8192
Bit 14 = dp  (decimal point)             = 16384
```

```
     ── a ──
    |\  |  /|
    f  h j k  b
    |  \|/  |
     g1  g2
    |  /|\  |
    e  n m l  c
    |/  |  \|
     ── d ──
                dp
```

Note: the datasheet labels the lower-left diagonal as N (not the more common M in some references). The diagonal segments run from corners toward the centre intersection.

### 3.5 Wave Tables

Select with `INL_WAVE_TBL, INL_PARAM_BASE + n`. Default is table 0 (sine).

| Index | Name | Description |
|-------|------|-------------|
| 0 | Sine | Smooth full-range oscillation. 0x00–0xFF. |
| 1 | Glint | Narrow bright spike on dim background. Movie-style highlight sweep. |
| 2 | Pulse | Hard on/off square wave. Half bright, half dim. |
| 3 | Sawtooth | Linear ramp 0x00–0xF8 then sharp drop. |
| 4 | Flame | Irregular low-mid values. Flickering firelight effect. |

To add custom tables: append 32 bytes to the `wave_tables` block in ROM and increment `WAVE_TBL_COUNT`.

### 3.6 Parameter Encoding

Parameters use range `0x90–0x9F`, encoding values 0–15:

```
actual_value = byte - 0x90
```

| Inline Code | Parameter Meaning | Range |
|-------------|-------------------|-------|
| `INL_BRIGHT` | brightness = (value + 1) × 16 - 1 | 0x0F – 0xFF |
| `INL_PAUSE` | duration in steps | 0 – 15 |
| `INL_SPEED` | speed = value + 1 ticks/step | 1 – 16 |
| `INL_WAVE_TBL` | table index | 0 – 15 |

---

## 4. Attribute Flags

Per-character flags stored in `attr_flags[]`:

| Bit | Name | Set By | Purpose |
|-----|------|--------|---------|
| 0 | `ATTR_F_FLASH` | `INL_FLASH` / `INL_NOFLASH` | Per-character flash toggle |
| 6 | `ATTR_F_SPEED` | `INL_SPEED` | Speed change marker (invisible entry) |
| 7 | `ATTR_F_PAUSE` | `INL_PAUSE` | Pause marker (invisible entry) |

Entries with `ATTR_F_SPEED` or `ATTR_F_PAUSE` set are skipped by all rendering functions. They occupy buffer slots but no display columns.

---

## 5. Hardware Interface

### 5.1 BIOS Routines

| Address | Name | Input | Function |
|---------|------|-------|----------|
| `0xFDD6` | `BIOS_SEG` | A=column, HL=segment bitmask | Write segment pattern |
| `0xFDD3` | `BIOS_BRT` | A=column, C=brightness | Write brightness |

**Warning:** Both routines may destroy any register including A. The engine uses explicit column counters pushed/popped around each call.

### 5.2 Constants

```z80
BIOS_SEG        equ     0FDD6h
BIOS_BRT        equ     0FDD3h
DISP_COLS       equ     24
MAX_BRIGHT      equ     128     ; adjust to hardware
```

### 5.3 Public API

| Routine | Input | Purpose |
|---------|-------|---------|
| `segfx_init` | HL = stream pointer | Initialise engine, load first page |
| `segfx_tick` | — | 50Hz tick handler (call from interrupt) |
| `segfx_update` | — | Push buffers to hardware if dirty (call from main loop) |
| `segfx_set_callback` | HL = callback address (0 = disable) | Register a page boundary callback |

### 5.4 Page Callback

A user-supplied function called after each page's exit transition completes, before the next page loads. Enables sound triggers, conditional page skipping, and game state integration.

**Registration:**

```z80
    ld      hl,my_handler
    call    segfx_set_callback      ; enable
    
    ld      hl,0
    call    segfx_set_callback      ; disable
```

**Callback contract:**

| | Detail |
|-|--------|
| **Called** | After each page's exit transition completes |
| **Input** | A = page index (1-based: 1 = first page just finished) |
| **Output** | A = 0 → continue to next page normally |
| | A ≠ 0 → skip the next page entirely |
| **May clobber** | AF, BC, DE, HL |
| **Preserved by engine** | IX, IY |

The page index resets to 0 when the stream loops. It increments at page load time, so the first page has index 1.

When a page is skipped, the engine scans forward past the next `PAGE_END` without loading or rendering the page. If `END_STREAM` is encountered during the skip, the stream wraps to the beginning.

**Example — sound trigger and conditional skip:**

```z80
my_handler:
    ; Play a sound after page 3
    cp      3
    jr      nz,.not3
    call    play_fanfare
    xor     a               ; A=0: continue normally
    ret

.not3:
    ; After page 4, skip page 5 unless high score is set
    cp      4
    jr      nz,.default
    ld      a,(high_score_flag)
    or      a
    jr      nz,.default     ; flag set: show page 5
    ld      a,1             ; A≠0: skip next page
    ret

.default:
    xor     a               ; continue
    ret
```

---

## 6. Memory Map

| Region | Size | Contents |
|--------|------|----------|
| Engine state | ~50 bytes | Pointers, counters, flags, callback, glyph counter |
| Display buffers | 144 bytes | disp_seg + disp_bright + disp_flash + shadow_seg |
| Text workspace | 768 bytes | text_buf + attr_bright + attr_flags |
| Custom char table | 49 bytes | 16 entries × 3 bytes + count |
| **Total RAM** | **~1011 bytes** | |
| Font table | 192 bytes ROM | 96 entries × 2 bytes |
| Wave tables | 160 bytes ROM | 5 × 32 bytes |
| Other tables | 34 bytes ROM | Dissolve order (24) + vfade masks (10) |
| Engine code | ~2.5 KB ROM | |
| Stream data | Variable | ROM or loaded |

---

## 7. Design Notes

**Register preservation:** BIOS calls destroy registers unpredictably. The engine never assumes register values survive a BIOS call.

**`ld d,0` hazard:** Using `add hl,de` for 16-bit indexing with `ld d,0` to zero-extend E will clobber D. The engine uses push/pop around these sequences when D holds loop state.

**Assembler parentheses:** In sjasmplus, `ld a,(EXPR)` is an indirect memory load. For immediate values, omit parentheses: `ld a,EXPR`. Arithmetic grouping like `ld a,(DISP_COLS / 2)` will load from memory address 12, not load the value 12.

**Flash composability:** Per-character flash is applied at output time, not in the brightness buffer. This means it composes correctly with all display effects.

**Scroll speed entries:** `INL_SPEED` creates invisible buffer entries. Placing one at the very start of scroll/bounce text creates a phantom blank column. Set initial speed in the page header instead.

---

## 8. Extension Points

- ~~Custom character definitions via inline codes~~ ✓ Implemented (see §3.4)
- Conditional page skip based on flag bytes (achievable now via callback)
- ~~Callback hooks at page boundaries~~ ✓ Implemented (see §5.4)
- Priority message interruption of current stream
- Composite pages with independent sub-page zones (see Appendix A)
- Additional wave tables (append 32 bytes, increment count)
- Extended character range `0xA0–0xFE` for custom glyphs

---

## Appendix A: Composite Pages (Design)

*Status: planned, not yet implemented.*

### A.1 Overview

A **composite page** divides the 24-column display into 2–4 horizontal zones, each running an independent display effect. This enables layouts like a static label alongside scrolling text, a score counter beside a flashing alert, or four independent status fields updating simultaneously.

```
  ┌──────────┬──────────────────────────────┐
  │ CONTROLS:│  <, > MOVE --- O FIRE ---    │  ← scrolling
  │ (static) │  P PAUSE --- H HELP ---      │
  └──────────┴──────────────────────────────┘
   cols 0–9        cols 10–23
   sub-page 0      sub-page 1
```

### A.2 Design Constraints

These constraints keep the implementation manageable on a Z80:

- **Maximum 4 sub-pages** per composite page.
- **Transitions are page-level only.** The entry and exit transitions operate on the full 24-column display. Sub-pages control only their display effect.
- **Shared speed.** All sub-pages advance on the same step timer. Individual sub-pages can still use `INL_SPEED` within their content to vary their own rate once independent step timers are justified, but v1 uses a single shared tick.
- **Independent dwell.** Each sub-page has its own dwell timer. The composite page exits (triggering the page-level exit transition) when **all** sub-pages have completed their dwell.
- **Sub-pages do not overlap.** Column ranges are non-overlapping and must tile the full display width (gaps appear as blank columns).

### A.3 Stream Format

#### Composite Page Header

```
0x01                ; PAGE_START
0x03                ; COMPOSITE marker (new)
<num_sub_pages>     ; 1 byte: 2–4
<entry_fx>          ; Page-level entry transition
<exit_fx>           ; Page-level exit transition
<speed>             ; Shared ticks-per-step
[SUB_PAGE 0]
[SUB_PAGE 1]
...
0x02                ; PAGE_END
```

The `0x03` byte immediately after `PAGE_START` distinguishes a composite page from a normal page (where this byte would be the entry transition code, which is never `0x03` since that code is unused in the transition table).

#### Sub-Page Block

Each sub-page is a self-contained block:

```
<col_start>         ; First column (0–23)
<col_count>         ; Width in columns (1–24)
<display_fx>        ; Display effect code (same codes as §3.2)
<dwell_lo>          ; Sub-page dwell low byte
<dwell_hi>          ; Sub-page dwell high byte
[CONTENT...]        ; Text and inline codes
0x02                ; SUB_PAGE_END (reuses PAGE_END marker)
```

Inline codes within a sub-page's content work as normal — `INL_BRIGHT`, `INL_FLASH`, `INL_ALIGN_*`, `INL_WAVE_TBL`, `INL_CHARDEF`, `INL_GLYPH`, and `INL_PAUSE` all apply within that sub-page's zone.

#### Example Stream

```
; "CONTROLS:" pinned left, instructions scroll right
01                      ; PAGE_START
03                      ; COMPOSITE
02                      ; 2 sub-pages
01                      ; entry: TRANS_FADE (page-level)
01                      ; exit: TRANS_FADE (page-level)
03                      ; speed: 3

; Sub-page 0: static label
00                      ; col_start: 0
0A                      ; col_count: 10
00                      ; DISP_STATIC
20 03                   ; dwell: 800 ticks (16 sec) [little-endian]
"CONTROLS: "
02                      ; SUB_PAGE_END

; Sub-page 1: scrolling instructions
0A                      ; col_start: 10
0E                      ; col_count: 14
03                      ; DISP_SCROLL_L
20 03                   ; dwell: 800 ticks (16 sec) [little-endian]
"<, > MOVE --- O FIRE --- P PAUSE --- H HELP"
02                      ; SUB_PAGE_END

02                      ; PAGE_END (composite)
```

### A.4 Sub-Page State

Each sub-page requires its own working state for independent effect animation:

```z80
                struct  SUB_PAGE_STATE
sp_fx_display   db      0       ; display effect code
sp_fx_pos       db      0       ; animation position
sp_fx_sub       db      0       ; sub-position (fade trails etc)
sp_dwell        dw      0       ; ticks remaining
sp_col_start    db      0       ; first display column
sp_col_count    db      0       ; zone width
sp_text_start   dw      0       ; offset into text_buf where this sub-page's content begins
sp_text_len     db      0       ; total decoded entries
sp_vis_len      db      0       ; visible character count
sp_text_offset  db      0       ; scroll position
sp_align_mode   db      0       ; alignment within zone
sp_bounce_dir   db      0       ; bounce state
sp_marquee_st   db      0       ; marquee state (0/1/2)
sp_marquee_hld  dw      0       ; marquee hold countdown
sp_wave_tbl     db      0       ; wave table index
sp_flash_phase  db      0       ; can share global or be independent
sp_done         db      0       ; 1 when dwell expired
                ends                    ; ~20 bytes per sub-page
```

Total per-sub-page overhead: ~20 bytes × 4 = **80 bytes** additional RAM.

The global engine state (`state.*`) retains page-level fields: `page_state`, `step_timer`, `speed`, `fx_entry`, `fx_exit`, `fx_pos`/`fx_sub` (for page-level transitions), `page_index`, `callback`. The per-sub-page fields move into the sub-page struct array.

For normal (non-composite) pages, the engine uses sub-page slot 0 with `col_start=0`, `col_count=24` — effectively a single full-width sub-page. This avoids duplicating the render path.

### A.5 Text Buffer Partitioning

Each sub-page's content is decoded into a sequential slice of the shared text workspace:

```
text_buf:      [sub0 content...][sub1 content...][sub2...][sub3...]
attr_bright:   [sub0 bright... ][sub1 bright... ][sub2...][sub3...]
attr_flags:    [sub0 flags...  ][sub1 flags...  ][sub2...][sub3...]
```

Each sub-page's `sp_text_start` records its base offset. With 256 bytes total and 4 sub-pages, the practical limit is ~64 characters per sub-page — adequate for most game display use cases. Scrolling text in narrow zones (10–14 columns) doesn't need long buffers since the visible window is small.

The decode pass processes sub-pages sequentially, advancing a write pointer through text_buf. Each sub-page's inline codes (brightness, flash, speed, pause) are resolved independently.

### A.6 Virtual Display Parametrisation

The key refactor: render functions operate on a **virtual display** rather than the physical 24 columns.

Two variables set before each render call:

```z80
vd_start:       db      0       ; first physical column for this zone
vd_cols:        db      24      ; number of columns in this zone
```

All render functions read `vd_start` and `vd_cols` instead of using `0` and `DISP_COLS`. Affected calculations:

| Function | Current | With virtual display |
|----------|---------|---------------------|
| `render_full` | Clears all 24 cols, writes from col 0 | Clears `vd_cols` cols from `vd_start` |
| `render_scrolled` | Wrap = `text_len + DISP_COLS` | Wrap = `text_len + vd_cols` |
| `calc_start_col` | Centre within 24 | Centre within `vd_cols`, offset by `vd_start` |
| `set_full_bright` | Sets cols 0–23 | Sets `vd_start` to `vd_start + vd_cols - 1` |
| Display effects | Index disp_seg/disp_bright from 0 | Index from `vd_start` |

The output routine (`segfx_output`) is unchanged — it always pushes all 24 columns to hardware.

### A.7 Tick Execution Model

During the **display phase** of a composite page:

```
for each sub_page (0..num_sub_pages-1):
    if sub_page.sp_done: continue

    ; Set virtual display bounds
    vd_start = sub_page.sp_col_start
    vd_cols  = sub_page.sp_col_count

    ; Load sub-page state into working registers
    ; (or reference via indexed pointer)

    ; Step the display effect
    call fx_display_step    ; uses vd_start/vd_cols

    ; Decrement dwell
    if sub_page.sp_dwell == 0:
        sub_page.sp_done = 1

; Check if all sub-pages are done
if all sp_done: transition to exit phase
```

Entry and exit transitions run at page level using the full display (vd_start=0, vd_cols=24), exactly as they do today.

### A.8 Completion and Callback

The composite page's exit transition begins when **all** sub-pages have exhausted their dwell. This means:

- Setting one sub-page's dwell much longer than another keeps it animating while the short one goes static (or repeats, depending on effect).
- Setting all dwells equal makes them finish together.
- The callback fires once after the page-level exit transition, as normal. `A` = page index.

For sub-pages with marquee effects, the marquee hold and scroll-out consume dwell time as they do today. The sub-page is "done" when its dwell reaches zero after the marquee has fully scrolled out.

### A.9 Backwards Compatibility

Normal pages (without the `0x03` composite marker) work exactly as before — the engine treats them as a single sub-page occupying the full display. No existing stream data needs to change.

### A.10 RAM Cost Summary

| Component | Normal page | Composite (4 sub-pages) |
|-----------|-------------|------------------------|
| Sub-page state | 0 (in engine state) | 80 bytes |
| Virtual display vars | 0 | 2 bytes |
| Text workspace | 768 bytes (shared) | 768 bytes (partitioned) |
| Custom char table | 49 bytes (shared) | 49 bytes (shared) |
| **Additional RAM** | — | **~82 bytes** |
| **Total engine RAM** | ~1011 bytes | ~1093 bytes |

### A.11 Timing Budget

At 8 MHz, the 20ms tick budget gives 160,000 T-states. Current worst case (scroll + output) is estimated under 15,000 T-states. Four sub-pages each doing a scroll render would be roughly 4× the render cost, so ~40,000–50,000 T-states — comfortably within budget.

### A.12 Implementation Order

Suggested order to minimise risk:

1. **Virtual display refactor.** Add `vd_start`/`vd_cols`, update all render functions to use them. Test with vd_start=0, vd_cols=24 — should be a no-op change.
2. **Sub-page state struct.** Define the struct, refactor normal pages to use sub-page slot 0.
3. **Composite page parsing.** New decode path for the `0x03` marker, multi-sub-page content decode.
4. **Display phase loop.** Iterate sub-pages, set virtual display, step each effect.
5. **Completion logic.** Track sp_done flags, trigger exit when all done.
6. **Testing.** Start with 2 sub-pages (static + scroll), then expand to 3 and 4.
