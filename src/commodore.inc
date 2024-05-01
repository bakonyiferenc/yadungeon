.ifndef _COMMODORE_INC_
_COMMODORE_INC_ = 1

;----------------------------------------------------------
;
;	Kernal vectors
;
;----------------------------------------------------------

CHROUT	=	$ffd2
GETIN	=	$ffe4

;----------------------------------------------------------
;
;	General macros & pseudocommands
;
;----------------------------------------------------------

; Prints a constant string
.macro	Print	string
	ldy	#0
Loop:	lda	Output,y
	beq	End
        jsr     CHROUT
	iny
	jmp	Loop
Output:	.text	string
	.byte	0
End:	
.endmacro

; Prints a char (stored in A)
.macro	PrintC	char
	lda	#char
        jsr     CHROUT
.endmacro

; Prints the decimal value of X
.macro	PrintX
	lda	#0
	jsr	$BDCD
.endmacro

; Prints the decimal value of A
.macro	PrintA
	tax
	PrintX
.endmacro

; Prints a 16 bit decimal value
.macro	PrintN16	arg
	ldx	arg
	lda	arg + 1
	jsr	$BDCD
.endmacro

; Prints a char at a given screen position
; X, Y: screen relative coordinates, A: character to print
.macro	PrintCXY	char
	mov	RowsLo, y : pos
	mov	RowsHi, y : pos+1
	lda	#char
	sta	pos:SCREENADDR, x
.endmacro

.macro	PrintAXY
	sta	char
	mov	RowsLo, y : pos
	mov	RowsHi, y : pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
.endmacro

RowsLo:
.for(var i = SCREENH - 1 ; i != 0 ; i--) .byte <(SCREENADDR + i * SCREENW)
RowsHi:
.for(var i = SCREENH - 1 ; i != 0 ; i--) .byte >(SCREENADDR + i * SCREENW)

; Returns key in A
.macro	GetKey
Wait:	jsr	GETIN
	cmp	#0
	beq	Wait
.endmacro

; Fill "len" bytes of memory starting from "addr" with "byte". If "len" is 0 then fill 256 bytes.
.macro	Fill	addr, byte, len
	ldx	#len - 1
	lda	#byte
Loop:	sta	addr, x
	dex
	bne	Loop
	sta	addr
.endmacro

; Copy "len" bytes of memory starting from "src" to "dst". If "len" is 0 then copy 256 bytes.
.macro	Copy	src, dst, len
	ldx	#len - 1
Loop:	mov	src, x : dst, x
	dex
	bne	Loop
	mov	src, x : dst, x
.endmacro

.pseudocommand mov src:tar {
	lda	src
	sta	tar
}

.function _16bitnextArgument(arg) {
	.if (arg.getType == AT_IMMEDIATE) 
		.return CmdArgument(arg.getType, >arg.getValue)
	.return CmdArgument(arg.getType, arg.getValue+1)
}

.pseudocommand mov16 src:tar {
	lda	src
	sta	tar
	lda	_16bitnextArgument(src)
	sta	_16bitnextArgument(tar)
}

.pseudocommand mov24 src:tar {
	.if (src.getType == AT_IMMEDIATE) {
		lda	src
		sta	tar
		lda #0
		sta tar.getValue+1
		sta tar.getValue+2
	} else {
		.error("Not implemented!")
	}
}

.pseudocommand add arg1 : arg2 : tar {
	.if (tar.getType == AT_NONE) .eval tar=arg1
	lda arg2
	clc
	adc arg1
	sta	tar
}

.pseudocommand add16 arg1 : arg2 : tar {
	.if (tar.getType == AT_NONE) .eval tar=arg1
	clc
	lda	arg1
	adc	arg2
	sta	tar
	.if (arg2.getType == AT_IMMEDIATE && (>arg2.getValue) == 0) {
		bcc skip
		inc tar.getValue+1
	skip:
	} else {
	lda	_16bitnextArgument(arg1)
	adc	_16bitnextArgument(arg2)
	sta	_16bitnextArgument(tar)
	}
}

.pseudocommand sub arg1 : arg2 : tar {
	.if (tar.getType == AT_NONE) .eval tar=arg1
	lda arg2
	sec
	sbc arg1
	sta	tar
}

.pseudocommand sub16 arg1 : arg2 : tar {
	.if (tar.getType == AT_NONE) .eval tar=arg1
	sec
	lda	arg1
	sbc	arg2
	sta	tar
	.if (arg2.getType == AT_IMMEDIATE && (>arg2.getValue) == 0) {
		bcs skip
		dec tar.getValue+1
	skip:
	} else {
	lda	_16bitnextArgument(arg1)
	sbc	_16bitnextArgument(arg2)
	sta	_16bitnextArgument(tar)
	}
}

.pseudocommand inc16 arg {
	inc	arg
	bne	over
	inc	arg.getValue + 1
over:
}

.pseudocommand inc24 arg {
	inc16	arg
	bne	over
	inc	arg.getValue + 2
over:
}

.endif  ; _COMMODORE_INC_