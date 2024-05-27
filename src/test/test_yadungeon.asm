.import	TestPrint

.segment "STARTUP"

Test:
.export	Test
		jsr	TestPrint
		rts