		OUTPUT	segfx.com
		ORG	0x0100

BIOS_TICK	EQU	0xFDC7		; HL -> handler

		LD	HL, example_stream
		CALL	segfx_init
		LD	HL, segfx_tick
		CALL	BIOS_TICK

loop		CALL	segfx_update
		JP	loop

		DEFINE	SEGFX_RAM_BASE $
		INCLUDE	"segfx.asm"	