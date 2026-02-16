# SEGFX — 14-Segment Display Effect Engine

## Protocol Specification v1.0

**Target:** Z80 microcomputer with 24× 14-segment character displays
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

**Dwell** is a 16-bit value in ticks (big-endian in stream). 50 ticks = 1 second. Max ~21 minutes.

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
<dwell_hi>      ; Dwell time high byte
<dwell_lo>      ; Dwell time low byte
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
| `0x01` | `TRANS_FADE` | 16 | All 24 columns fade brightness up (in) or down (out) simultaneously. Smooth 16-step ramp. |
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
| 0 | Top bars | `0x0001` |
| 1 | Upper verticals + mid horizontals | `0x0722` |
| 2 | Middle verticals | `0x00C0` |
| 3 | Lower verticals + diagonals | `0x3814` |
| 4 | Bottom bars | `0x4008` |

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

#### Important Usage Notes

**`INL_SPEED` at text start:** Creates a phantom blank column in scroll/bounce effects. Use the page header speed for the initial value; only use `INL_SPEED` for mid-text changes.

**`INL_FLASH` composability:** Applied at the output stage, not in display buffers. Composes correctly with any display effect — a breathing page with flash characters will have flash chars blink while others breathe.

**`INL_ALIGN_*` dual meaning:** For static/typewriter effects, controls text positioning. For scroll effects, controls initial blank offset. For marquee, controls the centred hold position.

### 3.4 Wave Tables

Select with `INL_WAVE_TBL, INL_PARAM_BASE + n`. Default is table 0 (sine).

| Index | Name | Description |
|-------|------|-------------|
| 0 | Sine | Smooth full-range oscillation. 0x00–0xFF. |
| 1 | Glint | Narrow bright spike on dim background. Movie-style highlight sweep. |
| 2 | Pulse | Hard on/off square wave. Half bright, half dim. |
| 3 | Sawtooth | Linear ramp 0x00–0xF8 then sharp drop. |
| 4 | Flame | Irregular low-mid values. Flickering firelight effect. |

To add custom tables: append 32 bytes to the `wave_tables` block in ROM and increment `WAVE_TBL_COUNT`.

### 3.5 Parameter Encoding

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
| Engine state | ~48 bytes | Pointers, counters, flags, callback |
| Display buffers | 144 bytes | disp_seg + disp_bright + disp_flash + shadow_seg |
| Text workspace | 768 bytes | text_buf + attr_bright + attr_flags |
| **Total RAM** | **~956 bytes** | |
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

- Custom character definitions via inline codes
- Conditional page skip based on flag bytes (achievable now via callback)
- ~~Callback hooks at page boundaries~~ ✓ Implemented (see §5.4)
- Priority message interruption of current stream
- Additional wave tables (append 32 bytes, increment count)
- Extended character range `0xA0–0xFE` for custom glyphs
