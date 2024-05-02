.ifdef __C64__
	.include "c64.inc"
.endif

.ifdef __C128__
	.include "c128.inc"
.endif

.ifdef __CX16__
	.include "cx16.inc"
.endif

.include	"smc.inc"

;----------------------------------------------------------
;
;	Game UI specific macros
;
;----------------------------------------------------------

; Prints a constant string to the message bar with -more- if needed
.macro	PrintMessage	string
	.local	Text, End
	mov16	#Text, Message
	mov	#End - Text, MessageSize

	jsr	_PrintMessage
	jmp	End
Text:	.byte	string

End:
.endmacro

;----------------------------------------------------------
;
;	Hash macros
;
;----------------------------------------------------------

.macro	HashA
	.local	vector
	sta	vector+1
vector:	lda	Hash
.endmacro

.macro	HashM	addr
	lda	addr
	HashA
.endmacro

.macro	HashAwithM	addr
	HashA
	eor	addr
	HashA
.endmacro

.macro	HashAwithX
	eor	Hash,x
.endmacro

.macro	HashAwithY
	eor	Hash,y
.endmacro

;----------------------------------------------------------
;
;	Yet Another Dungeon
;
;----------------------------------------------------------

.segment "STARTUP"
.segment "INIT"
.segment "ONCE"
.segment "CODE"

Start:	jsr	Init
SMC MainLoop, nop
	jsr	QuickStats
;	jsr	DrawScene
	jsr	PlayersTurn
	jsr	MonstersTurn
	jsr	IsPlayerAlive
	inc24	Turn
	jmp	MainLoop

;----------------------------------------------------------
;
;	Initialize
;
;----------------------------------------------------------

.proc	Init
	lda	#BLACK
	;sta	$d020
	;sta	$d021
	Print	"\$0e\$9b\$93Welcome to YAD!\nPress any key!\n"	; Lowercase, lightgray on black, clear screen
	GetKey
	PrintC	$93		; clr
	jsr	NewGame
	rts
.endproc

;----------------------------------------------------------
;
;	Start a new game
;
;----------------------------------------------------------

.proc	NewGame
	jsr	InitRnd
	jsr	InitHash

	ldx	#0
Loop:	lda	#0
	sta	Monster, x
	sta	MonsterX, x
	sta	MonsterY, x
	sta	MonsterHP, x
	sta	MonsterState1, x
	sta	MonsterState2, x
	mov	#$ff, {MonsterZ, x}	; Hoping all Monsters will (re)spawn till Player reaches dungeon level 255
	inx
	bne	Loop

	mov24	#1, Turn	; Reset turn counter

	mov	#100, PlayerHP	; Dummy character creation
	mov	#1, PlayerZ	; Start from City1
	
	jmp	EnterDungeon
.endproc

;----------------------------------------------------------
;
;	Player's turn
;
;----------------------------------------------------------

.proc	PlayersTurn
Loop:	GetKey
	sta	ZP_FREE3
	jsr	ClearMessage
	lda	ZP_FREE3
	jsr	ProcessCommand
	bcc	Loop		; Loop until a turn has passed
	rts
.endproc

;----------------------------------------------------------
;
;	Monsters are next
;
;----------------------------------------------------------

.proc	MonstersTurn
	rnd
	and	#7
	bne	End
	PrintMessage	"Monsters hit you!"
	dec	PlayerHP
End:	rts
.endproc

;----------------------------------------------------------
;
;	Is player still alive?
;
;----------------------------------------------------------

.proc	IsPlayerAlive
	lda	PlayerHP
	bne	Alive
Dead:	PrintMessage	"You died!"
	jmp	QuitGame
Alive:	rts
.endproc

;----------------------------------------------------------
;
;	Quit game
;
;----------------------------------------------------------

.proc	QuitGame
	mov	#RTS, MainLoop
	rts
.endproc

;----------------------------------------------------------
;
;	Print quickstats
;
;----------------------------------------------------------

.proc	QuickStats
	PrintC	$13		; home
	PrintC	$11		; down
	ldx	PlayerZ
	beq	Wilderness
	txa
	and	#~$07
	beq	Cities
	Print	"Dungeon   \n"
	jmp	Details
Cities:
	Print	"City      \n"
	jmp	Details
Wilderness:
	Print	"Wilderness\n"

Details:
	PrintX

	Print	"\nSize:\n"
	ldx	DungeonW
	PrintX
	PrintC	'x'
	ldx	DungeonH
	PrintX

	Print	" \nOffset:\n"
	ldx	OffsetX
	PrintX
	PrintC	'x'
	ldx	OffsetY
	PrintX
	Print	"  \n"

	PrintC	'@'
	Print	"\nx: "
	lda	PlayerX
	PrintA
	Print	" \ny: "
	lda	PlayerY
	PrintA
	Print	" \nz: "
	lda	PlayerZ
	PrintA
	Print	" \nHP: "
	lda	PlayerHP
	PrintA

	Print	" \nturn: "
	PrintN16	Turn
	rts
.endproc

;----------------------------------------------------------
;
;	Draw scene around player
;
;----------------------------------------------------------

; Draws the visible part of scene.
.proc	DrawVisibleScene
	ldx	PlayerX
	ldy	PlayerY
	iny
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	ldy	PlayerY
	dey
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	inx
	ldy	PlayerY
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	dex
	ldy	PlayerY
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	inx
	ldy	PlayerY
	iny
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	inx
	ldy	PlayerY
	dey
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	dex
	ldy	PlayerY
	dey
	jsr	RenderTile
	jsr	PrintDungeonTile
	ldx	PlayerX
	dex
	ldy	PlayerY
	iny
	jsr	RenderTile
	jsr	PrintDungeonTile
	rts
.endproc

;----------------------------------------------------------
;
; Scrolls the scene when needed.
; Must be called when Player{XY} was changed.
;
;----------------------------------------------------------

.proc	AdjustOffset
	lda	PlayerX
	sec
	sbc	OffsetX
	sbc	#SCENEW * 1 / 4
	bcc	AdjLeft
	cmp	#SCENEW * 2 / 4
	bcs	AdjRight
AdjY:	
	lda	PlayerY
	sec
	sbc	OffsetY
	sbc	#SCENEH * 1 / 4
	bcc	AdjDown
	cmp	#SCENEH * 2 / 4
	bcs	AdjUp
AdjEnd:	rts

AdjRight:
	lda	OffsetX
SMC ox1, { cmp #SMC_Value }
	beq	AdjY		; already at the far end, skip scroll
	clc
	adc	#SCENEW * 1 / 4
SMC ox2, { cmp #SMC_Value }
	bcc	:+
SMC ox3, { lda #SMC_Value }
:	jmp	ScrollLeft	; What about double change (Y)?

AdjLeft:
	lda	OffsetX
	beq	AdjY		; already at the far end, skip scroll
	sec
	sbc	#SCENEW * 1 / 4
	bcs	:+
	lda	#0
:	jmp	ScrollRight	; What about double change (Y)?

AdjUp:
	lda	OffsetY
SMC oy1, { cmp #SMC_Value }
	beq	AdjEnd		; already at the far end, skip scroll
	clc
	adc	#SCENEH * 1 / 4
SMC oy2, { cmp #SMC_Value }
	bcc	:+
SMC oy3, { lda #SMC_Value }
:	jmp	ScrollDown	; What about double change (Y)?

AdjDown:
	lda	OffsetY
	beq	AdjEnd		; already at the far end, skip scroll
	sec
	sbc	#SCENEH * 1 / 4
	bcs	:+
	lda	#0
:	jmp	ScrollUp	; What about double change (Y)?

.proc ScrollRight
	ldx	OffsetX		; X: old OffsetX
	sta	OffsetX		; A: new OffsetX
	SMC_StoreValue	ox, x
	sec
SMC ox, { sbc #SMC_Value }
	clc
	adc	#SCENEW - 1
	tax			; SCENEW - (X - A) - 1
	ldy	#SCENEW - 1
MoveColumn:
.repeat	::SCENEH, i
	POS .set ::SCREENADDR + ::SCENEOFFSET + (::SCENEH - i) * ::SCREENW
	mov	{POS,x}, {POS,y}
.endrep
	dey
	dex
	bmi	Done
	jmp	MoveColumn
Done:	lda	#' '
ClearColumn:
.repeat	::SCENEH, i
	sta	SCREENADDR + SCENEOFFSET + (SCENEH - i) * SCREENW, y
.endrep
	dey
	bmi	End
	jmp	ClearColumn
End:	jmp	Scrolled
.endproc

.proc ScrollLeft
	ldx	OffsetX		; X: old OffsetX
	sta	OffsetX		; A: new OffsetX
	SMC_StoreValue	ox, x
	sec
SMC ox, { sbc #SMC_Value }
	clc
	adc	#-SCENEW
	tax			; -SCENEW + (A - X)
	ldy	#-SCENEW
MoveColumn:
.repeat	::SCENEH, i
	POS .set ::SCREENADDR + ::SCENEOFFSET + (::SCENEH - i) * ::SCREENW + ::SCENEW - 256
	mov	{POS,x}, {POS,y}
.endrep
	iny
	inx
	bpl	Done
	jmp	MoveColumn
Done:	lda	#' '
ClearColumn:
.repeat	::SCENEH, i
	sta	SCREENADDR + SCENEOFFSET + (SCENEH - i) * SCREENW + SCENEW - 256, y
.endrep
	iny
	bpl	End
	jmp	ClearColumn
End:	jmp	Scrolled
.endproc

.macro	ScrollDownBy	n, exit
.repeat	::SCENEH - n, i
	POS1 .set ::SCREENADDR + ::SCENEOFFSET + (::SCENEH - i - n) * ::SCREENW - 1
	POS2 .set ::SCREENADDR + ::SCENEOFFSET + (::SCENEH - i)     * ::SCREENW - 1
	mov	{POS1,x}, {POS2,x}
.endrep
	lda	#' '
.repeat	n, i
	sta	SCREENADDR + SCENEOFFSET + (n -i) * SCREENW - 1, x
.endrep
	jmp	exit
.endmacro

.macro ScrollUpBy	n, exit
.repeat	::SCENEH - n, i
	POS1 .set ::SCREENADDR + ::SCENEOFFSET + (i - 1 + n) * ::SCREENW - 1
	POS2 .set ::SCREENADDR + ::SCENEOFFSET + (i - 1)     * ::SCREENW - 1
	mov	{POS1,x}, {POS2,x}
.endrep
	lda	#' '
.repeat	n, i
	sta	SCREENADDR + SCENEOFFSET + (SCENEH - n + i - 1) * SCREENW - 1, x
.endrep
	jmp	exit
.endmacro

.proc ScrollDown
	ldy	OffsetY		; Y: old OffsetY
	sta	OffsetY		; A: new OffsetY
	iny
	SMC_StoreValue	oy, y
	sec
SMC oy, { sbc #SMC_Value }
	asl
	clc
	adc	#<ScrollDownTable
	SMC_StoreLowByte	Scroll
	lda	#0
	adc	#>ScrollDownTable
	SMC_StoreHighByte	Scroll
	ldx	#SCENEW
Loop:
SMC Scroll, { jmp (SMC_AbsAdr) }
Next:	dex
	beq	Done
	jmp	Loop
Done:	jmp	Scrolled

ScrollDownTable:
	.word	ScrollDown1, ScrollDown2, ScrollDown3, ScrollDown4, ScrollDown5, ScrollDown6
ScrollDown1:
	ScrollDownBy	1, Next
ScrollDown2:
	ScrollDownBy	2, Next
ScrollDown3:
	ScrollDownBy	3, Next
ScrollDown4:
	ScrollDownBy	4, Next
ScrollDown5:
	ScrollDownBy	5, Next
ScrollDown6:
	ScrollDownBy	6, Next
.endproc

.proc ScrollUp
	ldy	OffsetY		; Y: old OffsetY
	sta	OffsetY		; A: new OffsetY
	SMC_StoreValue	oy
	dey
	tya
	sec
SMC oy, { sbc #SMC_Value }
	asl
	clc
	adc	#<ScrollUpTable
	SMC_StoreLowByte	Scroll
	lda	#0
	adc	#>ScrollUpTable
	SMC_StoreHighByte	Scroll
	ldx	#SCENEW
Loop:
SMC Scroll, { jmp (SMC_AbsAdr) }
Next:	dex
	beq	Done
	jmp	Loop
Done:	jmp	Scrolled

ScrollUpTable:
	.word	ScrollUp1, ScrollUp2, ScrollUp3, ScrollUp4, ScrollUp5, ScrollUp6
ScrollUp1:
	ScrollUpBy	1, Next
ScrollUp2:
	ScrollUpBy	2, Next
ScrollUp3:
	ScrollUpBy	3, Next
ScrollUp4:
	ScrollUpBy	4, Next
ScrollUp5:
	ScrollUpBy	5, Next
ScrollUp6:
	ScrollUpBy	6, Next
.endproc

Scrolled:
	jmp	OffsetUpdated
.endproc

;----------------------------------------------------------
;
; Draws the whole scene, visible or not.
;
;----------------------------------------------------------

.proc	DrawScene
	mov	#JSR_ABS, _tile
	mov16	#_RenderTile, _tile+1
	jmp	_DrawScene
.endproc

;----------------------------------------------------------
;
; Clears the whole scene
;
;----------------------------------------------------------

.proc	ClearScene
	mov	#NOP, _tile
	mov	#LDA_IMM, _tile+1
	mov	#' ', _tile+2
	; fall through _DrawScene
.endproc

.proc _DrawScene
SMC _screenLo, { lda #SMC_Value }
	SMC_StoreLowByte	screen
SMC _screenHi, { lda #SMC_Value }
	SMC_StoreHighByte	screen

SMC _y, { ldy #SMC_Value }
yloop:	dey
SMC _x, { ldx #SMC_Value }
xloop:	dex
_tile:	jsr	RenderTile
SMC screen, { sta SMC_AbsAdr, x }
SMC _ox, { cpx #SMC_Value }
	bne	xloop
	; add16
	clc
	SMC_LoadLowByte	screen
	adc	#SCREENW
	SMC_StoreLowByte	screen
	bcc	:+
	SMC_OperateOnHighByte inc, screen
:
SMC _oy, { cpy #SMC_Value }
	bne	yloop
	rts
.endproc

;----------------------------------------------------------
;
; Updates aux variables. 
; Must be called when Offset{XY} was changed.
;
;----------------------------------------------------------

.proc	OffsetUpdated
	sub	OffsetX, #<(SCREENADDR + SCENEOFFSET), _DrawScene::_screenLo
	lda	#>(SCREENADDR + SCENEOFFSET)
	sbc	#0
	SMC_StoreValue	_DrawScene::_screenHi

	add	OffsetX, #SCENEW, _DrawScene::_x
	add	OffsetY, #SCENEH, _DrawScene::_y

	mov	OffsetX, _DrawScene::_ox
	mov	OffsetY, _DrawScene::_oy
	rts
.endproc

;----------------------------------------------------------
;
;	Spawn a new monster
;	Input: X, Y: dungeon coordinates (0-255)
;	Returns a char representing the monster in A
;	Keeps: X, Y
;
;----------------------------------------------------------

.proc SpawnMonster
_X = ZP_FREE0
_Y = ZP_FREE1
	stx	_X
	sty	_Y

	ldx	MonsterCnt
	inc	MonsterCnt
	ldy	MonsterPtr
	inc	MonsterPtr
	mov	_X, {MonsterX, y}
	mov	_Y, {MonsterY, y}
	mov	PlayerZ, {MonsterZ, y}
	mov	#255, {MonsterHP, y}	; Max HP
	mov	#0, {MonsterState1, y}
	sta	MonsterState2, y
	tya
	sta	MonsterCache, x	
	rnd			; Monster type
	sta	Monster, y
	sta	LastMonster
	sty	LastMonsterIdx

	ldx	_X
	ldy	_Y
	rts
.endproc

;----------------------------------------------------------
;
;	Kill a monster
;	Input: X: monster idx
;
;----------------------------------------------------------

.proc KillMonster
	mov	#0, {MonsterHP, x}	; 0 HP == dead
	mov	#0, {MonsterState1, x}
	sta	MonsterState2, x
	lda	MonsterY, x
	tay
	lda	MonsterX, x
	tax
	jsr	RenderTile
	jsr	PrintDungeonTile
	PrintMessage	"It dies."
	rts
.endproc

;----------------------------------------------------------
;
;	Render a tile
;	Input: X, Y: dungeon coordinates (0-255)
;	Returns a char in A
;	Keeps: X, Y
;
;----------------------------------------------------------

.proc	RenderTile
	jsr	_RenderTile

	cmp	#'.'
	bne	End
	sta	ZP_FREE3
	rnd
	bne	NotSpawn	; Chance of a monster spawn = 1 : 256
	lda	MonsterCnt
	cmp	#20		; Max nr of monsters per dungeon
	bcs	NotSpawn
	jsr	SpawnMonster
	jmp	End
NotSpawn:
	lda	ZP_FREE3
End:	rts
.endproc

; A simple parametric room
.macro	RenderRoom xsize, ysize, chance
	.local	xc, yoffs, cmpx, cmpy, End
	txa
	and	#-xsize	; block size X: $f0 = 16, $f8 = 8, $fc = 4, etc
	HashA
	sta	xc+1

	tya
	and	#-ysize	; block size Y: $f0 = 16, $f8 = 8, $fc = 4, etc
xc:	eor	#SMC_Value
	HashAwithM	PlayerZ

	cmp	#256 * chance	; block chance: $40:$100 = 1:4
	bcs	End

	sta	yoffs+1
	and	#xsize/2 - 1
	sta	cmpx+1
	txa
	and	#xsize - 1
cmpx:	cmp	#SMC_Value
	bcc	End
yoffs:	lda	Rnd1
	and	#ysize/2 - 1
	sta	cmpy+1
	tya
	and	#ysize - 1
cmpy:	cmp	#SMC_Value
	bcc	End
	lda	#'.'		; Floor
	rts
End:
.endmacro

.proc _RenderTile
	jsr	RenderBorder
	jsr	RenderMonster

	cpx	#40		; Test tunnels
	bne	:+
	lda	#'.'
	rts
:	cpy	#10
	bne	:+
	lda	#'.'
	rts
:
	RenderRoom	16, 16, 1/30
	RenderRoom	8, 8, 1/16
	RenderRoom	16, 1, 1/8
	RenderRoom	1, 16, 1/8
	RenderRoom	4, 2, 1/8
	lda	#'#'		; Fall back to default rock
	rts
.endproc

; Impenetrable border around each landscape/dungeon
.proc	RenderBorder
	txa
	beq	Border
	cmp	DungeonMaxX
	bcs	Border
	tya
	beq	Border
	cmp	DungeonMaxY
	bcc	End
Border:	lda	#'%'		; Fence
End:	rts
.endproc

; Find an existing monster here
.proc RenderMonster
_X = 	ZP_FREE0
_Y = 	ZP_FREE1

	stx	_X
	sty	_Y

	ldx	MonsterCnt
	beq	NotFound
Loop:	dex
	lda	MonsterCache, x
	tay
	lda	MonsterX, y
	cmp	_X
	bne	Next
	lda	MonsterY, y
	cmp	_Y
	bne	Next

Found:	lda	MonsterHP, y
	beq	Next		; monster is dead

Alive:	lda	Monster, y
	sta	LastMonster
	sty	LastMonsterIdx
	ldx	_X
	ldy	_Y
	rts

Next:	cpx	#0
	bne	Loop
NotFound:
	ldx	_X
	ldy	_Y
	rts
.endproc
	
;----------------------------------------------------------
;
;	Process a command (stored in A)
;	Returns: C <- player completed a turn
;
;----------------------------------------------------------

ProcessCommand:
	cmp	#$11	; cursor down
	bne	:+
	jmp	CommandDown
:	cmp	#$91	; cursor up
	bne	:+
	jmp	CommandUp
:	cmp	#$1d	; cursor right
	bne	:+
	jmp	CommandRight
:	cmp	#$9d	; cursor left
	bne	:+
	jmp	CommandLeft
:	cmp	#'<'
	bne	:+
	jmp	Command_upstairs
:	cmp	#'>'
	bne	:+
	jmp	Command_downstairs
:
	tax
	cmp	#'z'+1
	bcs	IllegalCommand
	
	sec
	sbc	#'@'
	bcc	IllegalCommand

	asl
	SMC_StoreLowByte	Lookup
SMC Lookup, { jmp (CommandTable) }

;----------------------------------------------------------
;
;	Player commands
;
;----------------------------------------------------------

IllegalCommand:
	txa
	jsr	CHROUT
	PrintMessage	"Illegal command received"
	clc
	rts

.macro	MovePlayerBy	x, y
	.local	Wall, Floor
	ldx	PlayerX
	ldy	PlayerY
.if (x == -1)	dex
.if (x ==  1)	inx
.if (y == -1)	dey
.if (y ==  1)	iny
	jsr	RenderTile
	cmp	#'.'
	beq	Floor
	cmp	#'#'
	beq	Wall
	cmp	#'%'
	beq	Wall
	jmp	_Something
Wall:	jmp	_Wall
Floor:	jsr	HidePlayer
.if (x == -1)	dec	PlayerX
.if (x ==  1)	inc	PlayerX
.if (y == -1)	dec	PlayerY
.if (y ==  1)	inc	PlayerY
	jmp	_MovePlayerBy
.endmacro

_MovePlayerBy:
	jsr	AdjustOffset
	jsr	DrawVisibleScene
	jsr	PrintPlayer
	sec
	rts

_Wall:	PrintMessage	"There is a wall in the way!"
	clc
	rts

_Something:
	PrintMessage	"You hit something!"
	ldx	LastMonsterIdx
	jsr	KillMonster
	sec
	rts

CommandDown:
	MovePlayerBy	0, -1
CommandUp:
	MovePlayerBy	0, 1
CommandRight:
	MovePlayerBy	1, 0
CommandLeft:
	MovePlayerBy	-1, 0

CommandFoo:
	txa
	jsr	CHROUT
	PrintMessage	"Valid but unimplemented command received"
	sec
	rts

CommandSelf:	
	PrintMessage	"Printing character info"
	clc
	rts

Command_a:	
	PrintMessage	"Aim wand"
	sec
	rts

Command_l:
	PrintMessage	"Look around"
	jsr	DrawScene
	jsr	PrintPlayer
	clc
	rts

Command_q:	
	PrintMessage	"Quitting"
	jsr	QuitGame
	sec
	rts

Command_r:	
	PrintMessage	"Resting to full HP"
	mov	#250, PlayerHP
	sec
	rts

Command_z:	
	PrintMessage	"Zap rod"
	sec
	rts

Command_upstairs:
	dec	PlayerZ
	jsr	EnterDungeon
	sec
	rts

Command_downstairs:
	inc	PlayerZ
	jsr	EnterDungeon
	sec
	rts

.align	256
CommandTable:
.word	CommandSelf	; @
.word	Command_a	; a
.fillword	'l'-'b', CommandFoo
.word	Command_l
.fillword	'q'-'m', CommandFoo
.word	Command_q
.word	Command_r
.fillword	'z'-'s', CommandFoo
.word	Command_z	; z


;----------------------------------------------------------
;
;	Game UI
;
;----------------------------------------------------------

_more:	.byte "-more-"

.proc _PrintMessage
	lda	MessageCursor
	clc
	SMC_OperateOnValue	adc, MessageSize
	cmp	#SCREENW - _more.size
	bcc	:+
	Copy	_more, SCREENADDR + SCREENW - _more.size, _more.size
	GetKey
	jsr	ClearMessage
:	ldx	#0
:	lda	Message:$beef, x
	sta	MessageCursor:SCREENADDR
	inc	MessageCursor
	inx
SMC MessageSize, { cpx #SMC_Value }
	bne	:-
	inc	MessageCursor
	lda	MessageCursor
	rts
.endproc

.proc	ClearMessage
	Fill	SCREENADDR, ' ', SCREENW
	mov	#0, MessageCursor
	rts
.endproc

; Prints a char at given scene coordinates
; X, Y: scene relative coordinates, A: character to print
.proc	PrintSceneTile
	SMC_StoreValue	char
	mov	{SceneLo, y}, pos
	mov	{SceneHi, y}, pos+1
SMC char, { lda #SMC_Value }
	sta	pos:SCREENADDR, x
	rts
.endproc

; Prints a char at a given dungeon position
; X, Y: dungeon relative coordinates, A: character to print
.proc	PrintDungeonTile
	SMC_StoreValue	char
	txa
	sec
	sbc	OffsetX
	tax
	tya
	sec
	sbc	OffsetY
	tay
	mov	{SceneLo, y}, pos
	mov	{SceneHi, y}, pos+1
SMC char, { lda #SMC_Value }
	sta	pos:SCREENADDR, x
	rts
.endproc

; Replaces Player with underlying tile on screen
.proc	HidePlayer
	;ldx	PlayerX
	;ldy	PlayerY
	;jsr	RenderTile
	lda	#'.'
	ldx	PlayerX
	ldy	PlayerY
	jmp	PrintDungeonTile
.endproc

; Makes Player visible
.proc	PrintPlayer
	ldx	PlayerX
	ldy	PlayerY
	lda	#00		; '@'
	jmp	PrintDungeonTile
.endproc

SceneLo:
.repeat	::SCENEH, i
.byte .lobyte(SCREENADDR + SCENEOFFSET + (SCENEH - i) * SCREENW)
.endrep
SceneHi:
.repeat	::SCENEH, i
.byte .hibyte(SCREENADDR + SCENEOFFSET + (SCENEH - i) * SCREENW)
.endrep

;----------------------------------------------------------
;
;	Called upon entering a new level
;
;----------------------------------------------------------

.proc	EnterDungeon
	lda	#10		; Entry point
	sta	PlayerX
	sta	PlayerY
	lda	#2
	sta	OffsetX
	sta	OffsetY
	jsr	OffsetUpdated

	lda	PlayerZ
	bne	Regular		; Wilderness has a fixed size
	lda	#$ff
	sta	DungeonW
	sta	DungeonH
	lda	#$fe
	sta	DungeonMaxX
	sta	DungeonMaxY
	jmp	Monsters
Regular:			; Calculate regular dungeon size
	HashA
	tay
	and	PlayerZ
	ora	#20
	sta	DungeonW
	tax
	dex
	stx	DungeonMaxX
	tya
	and	PlayerZ
	ora	#16
	sta	DungeonH
	tax
	dex
	stx	DungeonMaxY

Monsters:			; Populate MonsterCache
	ldx	#0
	ldy	#0
	lda	PlayerZ
Loop:
	cmp	MonsterZ, x
	bne	NotHere
	lda	Monster, x
	txa
	sta	MonsterCache, y
	lda	PlayerZ
	iny
NotHere:
	inx
	bne	Loop
	sty	MonsterCnt

	lda	DungeonW	; For AdjustOffset
	sec
	sbc	#SCENEW
	SMC_StoreValue	ox1
	SMC_StoreValue	ox2
	SMC_StoreValue	ox3
	lda	DungeonH	; For AdjustOffset
	sec
	sbc	#SCENEH
	SMC_StoreValue	oy1
	SMC_StoreValue	oy2
	SMC_StoreValue	oy3
	
	jsr	ClearScene
	jsr	DrawVisibleScene
	jsr	PrintPlayer
	rts
.endproc

;----------------------------------------------------------
;
;	Random & Hash
;
;----------------------------------------------------------

.proc	InitRnd
	rnd
	sta	Seed
	rnd
	sta	Seed+1
	ldx	#0
Loop:	rnd
	sta	Rnd1, x
	rnd
	sta	Rnd2, x
	inx
	bne	Loop
	rts
.endproc

.proc	InitHash
	ldx	#0
	rnd
Loop:	cmp	#0
	beq	doEor
	asl
	beq	noEor	; if the input was $80, skip the EOR
	bcc	noEor
doEor:	eor	#$1d
noEor:	sta	Hash, x
	inx
	inx
	inx
	bne	Loop
	rts
.endproc

;----------------------------------------------------------
;
;	Game date to save
;
;----------------------------------------------------------

.align	256
Monster:	.fill 256, 0
MonsterX:	.fill 256, 0
MonsterY:	.fill 256, 0
MonsterZ:	.fill 256, 255	; Current dungeon: 0 = wilderness, 1-7 = cities, 8-255 = dungeons
MonsterHP:	.fill 256, 0
MonsterState1:	.fill 256, 0	; aggroed, sleeping, confused, stunned, feared, afraid, blind, deaf
MonsterState2:	.fill 256, 0	; poisoned, bleeding, fast/slow/paralyzed, drugged, invulnerable

Rnd1:		.fill 256, 0
Rnd2:		.fill 256, 0
Hash:		.fill 256, 0
Seed:		.word	0

MonsterPtr:	.byte	0	; Points to next free monster slot

PlayerX:	.byte	0
PlayerY:	.byte	0
PlayerZ:	.byte	0
PlayerHP:	.byte	0
PlayerState1:	.byte	0
PlayerState2:	.byte	0

Turn:		.byte	0, 0, 0
OffsetX:	.byte	0	; To center the dungeon around the player
OffsetY:	.byte	0

;----------------------------------------------------------
;
;	Cached data (not to save)
;
;----------------------------------------------------------

DungeonW:	.byte	0	; Size of current dungeon
DungeonH:	.byte	0
DungeonMaxX:	.byte	0
DungeonMaxY:	.byte	0
MonsterCnt:	.byte	0	; Nr of monsters at current dungeon
LastMonster:	.byte 	0	; Most recent monster
LastMonsterIdx:	.byte 	0	; Index of most recent monster

.align 256

MonsterCache:	.fill 256, 0	; Pointers to monsters at current dungeon