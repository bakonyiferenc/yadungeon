.import	Print

TestPrint:
.export	TestPrint
		jsr	Print
		.asciiz	"print is working!"
		rts