.import	Print
.feature string_escapes

.segment "STARTUP"
		lda	#0		; Black
		sta	$d020		; Border
		sta	$d021		; Screen
		jsr	Print
		.byte	$0e, $9b, $93	; Lowercase, lightgray, clear screen
		.asciiz	"Welcome to YAD!\nPress any key!\n"
		rts