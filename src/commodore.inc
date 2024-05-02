.ifndef _COMMODORE_INC_
_COMMODORE_INC_ = 1

.feature force_range, string_escapes

;----------------------------------------------------------
;
;	Kernal vectors
;
;----------------------------------------------------------

CHROUT	=	$ffd2
GETIN	=	$ffe4

;----------------------------------------------------------
;
;	General macros
;
;----------------------------------------------------------

; Prints a constant string
.macro	Print	string
        .local  Loop, Output, End
	ldy	#0
Loop:	lda	Output,y
	beq	End
        jsr     CHROUT
	iny
	jmp	Loop
Output:	.asciiz	string
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

; Returns key in A
.macro	GetKey
        .local  Wait
Wait:	jsr	GETIN
	cmp	#0
	beq	Wait
.endmacro

; Fill "len" bytes of memory starting from "addr" with "byte". If "len" is 0 then fill 256 bytes.
.macro	Fill	addr, byte, len
        .local  Loop
	ldx	#len - 1
	lda	#byte
Loop:	sta	addr, x
	dex
	bne	Loop
	sta	addr
.endmacro

; Copy "len" bytes of memory starting from "src" to "dst". If "len" is 0 then copy 256 bytes.
.macro	Copy	src, dst, len
        .local  Loop
	ldx	#len - 1
Loop:	mov	{src, x}, {dst, x}
	dex
	bne	Loop
	mov	{src, x}, {dst, x}
.endmacro

.macro  mov src, dest
    lda src
    sta dest
.endmacro

.macro  mov16    src, dest
    .local sepd, seps
    sepd .set 0
    seps .set 0

    .if .match({.right(2,{dest})},{,x}) .or .match({.right(2,{dest})},{,y})
        sepd .set 2
    .endif
    .if .match({.right(2,{src})},{,x}) .or .match({.right(2,{src})},{,y})
        seps .set 2
    .endif

    .if .match(.left(1, {src}),#)
        mov #.lobyte(.right(.tcount({src})-1,{src})), {.left(.tcount({dest})-sepd,dest)+0 .right(sepd,dest)}
        mov #.hibyte(.right(.tcount({src})-1,{src})), {.left(.tcount({dest})-sepd,dest)+1 .right(sepd,dest)}
    .else
        mov {.left(.tcount({src})-seps,src)+0 .right(seps,src)}, {.left(.tcount({dest})-sepd,dest)+0 .right(sepd,dest)}
        mov {.left(.tcount({src})-seps,src)+1 .right(seps,src)}, {.left(.tcount({dest})-sepd,dest)+1 .right(sepd,dest)}
    .endif
.endmacro

.macro  mov24   src, dest
    .local sepd, seps
    sepd .set 0
    seps .set 0

    .if .match({.right(2,{dest})},{,x}) .or .match({.right(2,{dest})},{,y})
        sepd .set 2
    .endif
    .if .match({.right(2,{src})},{,x}) .or .match({.right(2,{src})},{,y})
        seps .set 2
    .endif

    .if .match(.left(1, {src}),#)
        mov #.lobyte(.right(.tcount({src})-1,{src})), {.left(.tcount({dest})-sepd,dest)+0 .right(sepd,dest)}
        mov #.hibyte(.right(.tcount({src})-1,{src})), {.left(.tcount({dest})-sepd,dest)+1 .right(sepd,dest)}
        mov #.bankbyte(.right(.tcount({src})-1,{src})), {.left(.tcount({dest})-sepd,dest)+2 .right(sepd,dest)}
    .else
        mov {.left(.tcount({src})-seps,src)+0 .right(seps,src)}, {.left(.tcount({dest})-sepd,dest)+0 .right(sepd,dest)}
        mov {.left(.tcount({src})-seps,src)+1 .right(seps,src)}, {.left(.tcount({dest})-sepd,dest)+1 .right(sepd,dest)}
        mov {.left(.tcount({src})-seps,src)+2 .right(seps,src)}, {.left(.tcount({dest})-sepd,dest)+2 .right(sepd,dest)}
    .endif
.endmacro

.macro  add     arg1, arg2, dest
	lda arg2
	clc
	adc arg1
	sta	dest
.endmacro

.macro  add16   immediate, dest
        .local  skip
	clc
	lda	dest
	adc	immediate
	sta	dest
	bcc skip
	inc dest+1
skip:
.endmacro

.macro  sub     arg1, arg2, dest
	lda arg2
	sec
	sbc arg1
	sta	dest
.endmacro

.macro  inc16   arg
        .local  over
	inc	arg
	bne	over
	inc	arg+1
over:
.endmacro


.macro  inc24   arg
        .local  over
	inc16	arg
	bne	over
	inc	arg+2
over:
.endmacro

.endif  ; _COMMODORE_INC_