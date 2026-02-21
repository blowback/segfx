		OUTPUT	"pwm_test.com"
		ORG	0x0100
		
disp_bitmask	EQU	0xFDD6	; HL=bits, A = col 0..23
disp_bright	EQU	0xFDD3	; C=brightness, A = col 0..23

	MACRO	CHAR col?, bright?
		LD	HL, 0xFFFF
		LD	A, col?
		CALL	disp_bitmask

		LD	C, bright?
		LD	A, col?
		CALL	disp_bright
	ENDM
		
	DUP	24, idx
		CHAR	idx, idx * (256/24)
	EDUP
		