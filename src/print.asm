.include "smc.inc"

CHROUT	=	$ffd2

; Prints a zero-terminated string stored behind the jsr
; 		jsr	Print
;		.asciiz	"Hello, world!"	; Maximum of 255 bytes (including trailing 0)
;		nop	; Print returns here
; Destroys: A, X
.proc Print
.export Print
		pla				; String is stored right after "jsr Print"
		SMC_StoreLowByte StrAddr	; so we can pop the address of the string
		pla				; from the stack and store it at label: StrAddr
		SMC_StoreHighByte StrAddr

		ldx	#0			; X holds the current position within the string
Loop:
SMC StrAddr, {	lda	SMC_AbsAdr, x }
		beq	LoopEnd			; String is 0 terminated
		jsr	CHROUT			; Prints one character stored in A
		inx
		jmp	Loop
LoopEnd:
		txa				; X now contains the length of the string
		clc				; Adjust the return address
		SMC_OperateOnLowByte adc, StrAddr
		tax				; by adding the size of the string
			SMC_LoadHighByte StrAddr
			adc	#0
			pha			; and push it into the stack
		txa
		pha
		rts				; so we can return to the next address
.endproc					; after the trailing 0 of the string
