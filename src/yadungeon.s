.ifdef __C64__
	.include "c64.inc"
.endif

.ifdef __C128__
	.include "c128.inc"
.endif

.ifdef __CX16__
	.include "cx16.inc"
.endif

.segment "STARTUP"
.segment "INIT"
.segment "ONCE"
.segment "CODE"

;----------------------------------------------------------
;
;	Yet Another Dungeon
;
;----------------------------------------------------------

Start:	Init()
MainLoop:			; <- self modifying
	QuickStats()
;	DrawScene()
	PlayersTurn()
	MonstersTurn()
	IsPlayerAlive()
	inc24	Turn
	jmp	MainLoop

;----------------------------------------------------------
;
;	Initialize
;
;----------------------------------------------------------

.macro	Init() {
	lda	#BLACK
	;sta	$d020
	;sta	$d021
	Print(@"\$0e\$9b\$93Welcome to YAD!\nPress any key!\n")	; Lowercase, lightgray on black, clear screen
	GetKey()
	PrintC($93)		; clr
	NewGame()
}

;----------------------------------------------------------
;
;	Start a new game
;
;----------------------------------------------------------

.macro	NewGame() {
	InitRnd()
	InitHash()

	ldx	#0
Loop:	lda	#0
	sta	Monster, x
	sta	MonsterX, x
	sta	MonsterY, x
	sta	MonsterHP, x
	sta	MonsterState1, x
	sta	MonsterState2, x
	mov	#$ff : MonsterZ, x	; Hoping all Monsters will (re)spawn till Player reaches dungeon level 255
	inx
	bne	Loop

	mov24	#1 : Turn	; Reset turn counter

	mov	#100 : PlayerHP	; Dummy character creation
	mov	#1 : PlayerZ	; Start from City1
	
	EnterDungeon()
}

;----------------------------------------------------------
;
;	Player's turn
;
;----------------------------------------------------------

.macro	PlayersTurn() {
Loop:	GetKey()
	sta	ZP_FREE3
	ClearMessage()
	lda	ZP_FREE3
	jsr	ProcessCommand
	bcc	Loop		; Loop until a turn has passed
}

;----------------------------------------------------------
;
;	Monsters are next
;
;----------------------------------------------------------

.macro	MonstersTurn() {
	rnd
	and	#7
	bne	End
	PrintMessage("Monsters hit you!")
	dec	PlayerHP
End:
}

;----------------------------------------------------------
;
;	Is player still alive?
;
;----------------------------------------------------------

.macro	IsPlayerAlive() {
	lda	PlayerHP
	bne	Alive
Dead:	PrintMessage("You died!")
	QuitGame()
Alive:
}

;----------------------------------------------------------
;
;	Quit game
;
;----------------------------------------------------------

.macro	QuitGame() {
	mov	#RTS : MainLoop
}

;----------------------------------------------------------
;
;	Print quickstats
;
;----------------------------------------------------------

.macro	QuickStats() {
	PrintC($13)		; home
	PrintC($11)		; down
	ldx	PlayerZ
	beq	Wilderness
	txa
	and	#~$07
	beq	Cities
	Print(@"Dungeon   \n")
	jmp	Details
Cities:
	Print(@"City      \n")
	jmp	Details
Wilderness:
	Print(@"Wilderness\n")

Details:
	PrintX()

	Print(@"\nSize:\n")
	ldx	DungeonW
	PrintX()
	PrintC('x')
	ldx	DungeonH
	PrintX()

	Print(@" \nOffset:\n")
	ldx	OffsetX
	PrintX()
	PrintC('x')
	ldx	OffsetY
	PrintX()
	Print(@"  \n")

	PrintC('@')
	Print(@"\nx: ")
	lda	PlayerX
	PrintA()
	Print(@" \ny: ")
	lda	PlayerY
	PrintA()
	Print(@" \nz: ")
	lda	PlayerZ
	PrintA()
	Print(@" \nHP: ")
	lda	PlayerHP
	PrintA()

	Print(@" \nturn: ")
	PrintN16(Turn)
}

;----------------------------------------------------------
;
;	Draw scene around player
;
;----------------------------------------------------------

; Draws the visible part of scene.
.macro	DrawVisibleScene() {
	jsr	_DrawVisibleScene
}

_DrawVisibleScene:
	ldx	PlayerX
	ldy	PlayerY
	iny
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	ldy	PlayerY
	dey
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	inx
	ldy	PlayerY
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	dex
	ldy	PlayerY
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	inx
	ldy	PlayerY
	iny
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	inx
	ldy	PlayerY
	dey
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	dex
	ldy	PlayerY
	dey
	RenderTile()
	PrintDungeonTile()
	ldx	PlayerX
	dex
	ldy	PlayerY
	iny
	RenderTile()
	PrintDungeonTile()
	rts

;----------------------------------------------------------
;
; Updates aux variables. 
; Must be called when Offset{XY} was changed.
;
;----------------------------------------------------------

.macro	OffsetUpdated() {
	sub OffsetX : #<(SCREENADDR + SCENEOFFSET) : _screenLo
	lda	#>(SCREENADDR + SCENEOFFSET)
	sbc	#0
	sta	_screenHi

	add	OffsetX : #SCENEW : _x
	add	OffsetY : #SCENEH : _y

	mov	OffsetX : _ox
	mov	OffsetY : _oy
}

;----------------------------------------------------------
;
; Scrolls the scene when needed.
; Must be called when Player{XY} was changed.
;
;----------------------------------------------------------

.macro	AdjustOffset() {
	jsr	_AdjustOffset
}

_AdjustOffset:
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
	cmp	ox1:#0
	beq	AdjY		; already at the far end, skip scroll
	clc
	adc	#SCENEW * 1 / 4
	cmp	ox2:#0
	bcc	!+
	lda	ox3:#0
!:	jmp	ScrollLeft	; What about double change (Y)?

AdjLeft:
	lda	OffsetX
	beq	AdjY		; already at the far end, skip scroll
	sec
	sbc	#SCENEW * 1 / 4
	bcs	!+
	lda	#0
!:	jmp	ScrollRight	; What about double change (Y)?

AdjUp:
	lda	OffsetY
	cmp	oy1:#0
	beq	AdjEnd		; already at the far end, skip scroll
	clc
	adc	#SCENEH * 1 / 4
	cmp	oy2:#0
	bcc	!+
	lda	oy3:#0
!:	jmp	ScrollDown	; What about double change (Y)?

AdjDown:
	lda	OffsetY
	beq	AdjEnd		; already at the far end, skip scroll
	sec
	sbc	#SCENEH * 1 / 4
	bcs	!+
	lda	#0
!:	jmp	ScrollUp	; What about double change (Y)?

ScrollRight: {
	ldx	OffsetX		; X: old OffsetX
	sta	OffsetX		; A: new OffsetX
	stx	x
	sec
	sbc	x:#0
	clc
	adc	#SCENEW - 1
	tax			; SCENEW - (X - A) - 1
	ldy	#SCENEW - 1
MoveColumn:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) {
	.var POS = SCREENADDR + SCENEOFFSET + i * SCREENW
	mov	POS,x : POS,y
}
	dey
	dex
	bmi	Done
	jmp	MoveColumn
Done:	lda	#' '
ClearColumn:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) {
	sta	SCREENADDR + SCENEOFFSET + i * SCREENW, y
}
	dey
	bmi	End
	jmp	ClearColumn
End:	jmp	Scrolled
}

ScrollLeft: {
	ldx	OffsetX		; X: old OffsetX
	sta	OffsetX		; A: new OffsetX
	stx	x
	sec
	sbc	x:#0
	clc
	adc	#-SCENEW
	tax			; -SCENEW + (A - X)
	ldy	#-SCENEW
MoveColumn:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) {
	.var POS = SCREENADDR + SCENEOFFSET + i * SCREENW + SCENEW - 256
	mov	POS,x : POS,y
}
	iny
	inx
	bpl	Done
	jmp	MoveColumn
Done:	lda	#' '
ClearColumn:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) {
	sta	SCREENADDR + SCENEOFFSET + i * SCREENW + SCENEW - 256, y
}
	iny
	bpl	End
	jmp	ClearColumn
End:	jmp	Scrolled
}

.macro	ScrollDownBy(n, exit) {
.for(var i = SCENEH - 1 ; i >= n ; i--) {
	.var POS1 = SCREENADDR + SCENEOFFSET + (i - n) * SCREENW - 1
	.var POS2 = SCREENADDR + SCENEOFFSET +  i      * SCREENW - 1
	mov	POS1,x : POS2,x
}
	lda	#' '
.for(var i = n - 1 ; i >= 0 ; i--) {
	sta	SCREENADDR + SCENEOFFSET + i * SCREENW - 1, x
}
	jmp	exit
}

.macro ScrollUpBy(n, exit) {
.for(var i = 0 ; i < SCENEH - n ; i++) {
	.var POS1 = SCREENADDR + SCENEOFFSET + (i + n) * SCREENW - 1
	.var POS2 = SCREENADDR + SCENEOFFSET +  i      * SCREENW - 1
	mov	POS1,x : POS2,x
}
	lda	#' '
.for(var i = SCENEH - n ; i < SCENEH ; i++) {
	sta	SCREENADDR + SCENEOFFSET + i * SCREENW - 1, x
}
	jmp	exit
}

ScrollDown: {
	ldy	OffsetY		; Y: old OffsetY
	sta	OffsetY		; A: new OffsetY
	iny
	sty	y
	sec
	sbc	y:#0
	asl
	clc
	adc	#<ScrollDownTable
	sta	Scroll
	lda	#0
	adc	#>ScrollDownTable
	sta	Scroll+1
	ldx	#SCENEW
Loop:	jmp	Scroll:($beef)
Next:	dex
	beq	Done
	jmp	Loop
Done:	jmp	Scrolled

ScrollDownTable:
	.word	ScrollDown1, ScrollDown2, ScrollDown3, ScrollDown4, ScrollDown5, ScrollDown6
ScrollDown1:
	ScrollDownBy(1, Next)
ScrollDown2:
	ScrollDownBy(2, Next)
ScrollDown3:
	ScrollDownBy(3, Next)
ScrollDown4:
	ScrollDownBy(4, Next)
ScrollDown5:
	ScrollDownBy(5, Next)
ScrollDown6:
	ScrollDownBy(6, Next)
}

ScrollUp: {
	ldy	OffsetY		; Y: old OffsetY
	sta	OffsetY		; A: new OffsetY
	sta	a
	dey
	tya
	sec
	sbc	a:#0
	asl
	clc
	adc	#<ScrollUpTable
	sta	Scroll
	lda	#0
	adc	#>ScrollUpTable
	sta	Scroll+1
	ldx	#SCENEW
Loop:	jmp	Scroll:($beef)
Next:	dex
	beq	Done
	jmp	Loop
Done:	jmp	Scrolled

ScrollUpTable:
	.word	ScrollUp1, ScrollUp2, ScrollUp3, ScrollUp4, ScrollUp5, ScrollUp6
ScrollUp1:
	ScrollUpBy(1, Next)
ScrollUp2:
	ScrollUpBy(2, Next)
ScrollUp3:
	ScrollUpBy(3, Next)
ScrollUp4:
	ScrollUpBy(4, Next)
ScrollUp5:
	ScrollUpBy(5, Next)
ScrollUp6:
	ScrollUpBy(6, Next)
}

Scrolled:
	OffsetUpdated()
	rts

;----------------------------------------------------------
;
; Draws the whole scene, visible or not.
;
;----------------------------------------------------------

.macro	DrawScene() {
	mov	#JSR_ABS : _tile
	mov16 #_RenderTile : _tile+1
	jsr	_DrawScene
}

;----------------------------------------------------------
;
; Clears the whole scene
;
;----------------------------------------------------------

.macro	ClearScene() {
	mov	#NOP : _tile
	mov	#LDA_IMM : _tile+1
	mov	#' ' : 	_tile+2
	jsr	_DrawScene
}

_DrawScene:
	lda	_screenLo:#0
	sta	screen
	lda	_screenHi:#0
	sta	screen+1

	ldy	_y:#0
y:	dey
	ldx	_x:#0
x:	dex
_tile:	RenderTile()
	sta	screen:SCREENADDR, x
	cpx	_ox:#0
	bne	x
	add16 screen : #SCREENW
	cpy	_oy:#0
	bne	y
	rts

;----------------------------------------------------------
;
;	Spawn a new monster
;	Input: X, Y: dungeon coordinates (0-255)
;	Returns a char representing the monster in A
;	Keeps: X, Y
;
;----------------------------------------------------------

.macro SpawnMonster() {
	jsr	_SpawnMonster
}

_SpawnMonster: {
X = ZP_FREE0
Y = ZP_FREE1
	stx	X
	sty	Y

	ldx	MonsterCnt
	inc	MonsterCnt
	ldy	MonsterPtr
	inc	MonsterPtr
	mov	X : MonsterX, y
	mov	Y : MonsterY, y
	mov	PlayerZ : MonsterZ, y
	mov	#255 : MonsterHP, y	; Max HP
	mov	#0 : MonsterState1, y
	sta	MonsterState2, y
	tya
	sta	MonsterCache, x	
	rnd			; Monster type
	sta	Monster, y
	sta	LastMonster
	sty	LastMonsterIdx

	ldx	X
	ldy	Y
	rts
}

;----------------------------------------------------------
;
;	Kill a monster
;	Input: X: monster idx
;
;----------------------------------------------------------

.macro KillMonster() {
	mov	#0 : MonsterHP, x	; 0 HP == dead
	mov	#0 : MonsterState1, x
	sta	MonsterState2, x
	lda	MonsterY, x
	tay
	lda	MonsterX, x
	tax
	RenderTile()
	PrintDungeonTile()
	PrintMessage("It dies.")
}

;----------------------------------------------------------
;
;	Render a tile
;	Input: X, Y: dungeon coordinates (0-255)
;	Returns a char in A
;	Keeps: X, Y
;
;----------------------------------------------------------

.macro	RenderTile() {
	jsr	_RenderTile

	cmp	#'.'
	bne	End
	sta	ZP_FREE3
	rnd
	bne	NotSpawn	; Chance of a monster spawn = 1 : 256
	lda	MonsterCnt
	cmp	#20		; Max nr of monsters per dungeon
	bcs	NotSpawn
	SpawnMonster()
	jmp	End
NotSpawn:
	lda	ZP_FREE3
End:
}

_RenderTile:
	RenderBorder()
	RenderMonster()

	cpx	#40		; Test tunnels
	bne	!+
	lda	#'.'
	rts
!:	cpy	#10
	bne	!+
	lda	#'.'
	rts
!:
	RenderRoom(16, 16, 1/30)
	RenderRoom(8, 8, 1/16)
	RenderRoom(16, 1, 1/8)
	RenderRoom(1, 16, 1/8)
	RenderRoom(4, 2, 1/8)
	lda	#'#'		; Fall back to default rock
	rts

; Impenetrable border around each landscape/dungeon
.macro	RenderBorder() {
	txa
	beq	Border
	cmp	DungeonMaxX
	bcs	Border
	tya
	beq	Border
	cmp	DungeonMaxY
	bcc	End
Border:	lda	#'%'		; Fence
	rts
End:
}

; Find an existing monster here
.macro RenderMonster() {
X = 	ZP_FREE0
Y = 	ZP_FREE1

	stx	X
	sty	Y

	ldx	MonsterCnt
	beq	NotFound
Loop:	dex
	lda	MonsterCache, x
	tay
	lda	MonsterX, y
	cmp	X
	bne	Next
	lda	MonsterY, y
	cmp	Y
	bne	Next

Found:	lda	MonsterHP, y
	beq	Next		; monster is dead

Alive:	lda	Monster, y
	sta	LastMonster
	sty	LastMonsterIdx
	ldx	X
	ldy	Y
	rts

Next:	cpx	#0
	bne	Loop
NotFound:
	ldx	X
	ldy	Y
}


; A simple parametric room
.macro	RenderRoom(xsize, ysize, chance) {
	txa
	and	#-xsize	; block size X: $f0 = 16, $f8 = 8, $fc = 4, etc
	HashA()
	sta	xc

	tya
	and	#-ysize	; block size Y: $f0 = 16, $f8 = 8, $fc = 4, etc
	eor	xc:#0
	HashAwithM(PlayerZ)

	cmp	#256 * chance	; block chance: $40:$100 = 1:4
	bcs	End

	sta	yoffs
	and	#xsize/2 - 1
	sta	cmpx
	txa
	and	#xsize - 1
	cmp	cmpx:#0
	bcc	End
	lda	yoffs:Rnd1
	and	#ysize/2 - 1
	sta	cmpy
	tya
	and	#ysize - 1
	cmp	cmpy:#0
	bcc	End
	lda	#'.'		; Floor
	rts
End:
}	
	
;----------------------------------------------------------
;
;	Process a command (stored in A)
;	Returns: C <- player completed a turn
;
;----------------------------------------------------------

ProcessCommand:
	cmp	#$11	; cursor down
	bne	!+
	jmp	CommandDown
!:	cmp	#$91	; cursor up
	bne	!+
	jmp	CommandUp
!:	cmp	#$1d	; cursor right
	bne	!+
	jmp	CommandRight
!:	cmp	#$9d	; cursor left
	bne	!+
	jmp	CommandLeft
!:	cmp	#'<'
	bne	!+
	jmp	Command_upstairs
!:	cmp	#'>'
	bne	!+
	jmp	Command_downstairs
!:
	tax
	cmp	#'z'+1
	bcs	IllegalCommand
	
	sec
	sbc	#'@'
	bcc	IllegalCommand

	asl
	sta	Lookup
	jmp	Lookup:(CommandTable)

;----------------------------------------------------------
;
;	Player commands
;
;----------------------------------------------------------

IllegalCommand:
	txa
	jsr	CHROUT
	PrintMessage("Illegal command received")
	clc
	rts

.macro	MovePlayerBy(x, y) {
	ldx	PlayerX
	ldy	PlayerY
.if (x == -1)	dex
.if (x ==  1)	inx
.if (y == -1)	dey
.if (y ==  1)	iny
	RenderTile()
	cmp	#'.'
	beq	Floor
	cmp	#'#'
	beq	Wall
	cmp	#'%'
	beq	Wall
	jmp	_Something
Wall:	jmp	_Wall
Floor:	HidePlayer()
.if (x == -1)	dec	PlayerX
.if (x ==  1)	inc	PlayerX
.if (y == -1)	dec	PlayerY
.if (y ==  1)	inc	PlayerY
	jmp	_MovePlayerBy
}

_MovePlayerBy:
	AdjustOffset()
	DrawVisibleScene()
	PrintPlayer()
	sec
	rts

_Wall:	PrintMessage("There is a wall in the way!")
	clc
	rts

_Something:
	PrintMessage("You hit something!")
	ldx	LastMonsterIdx
	KillMonster()
	sec
	rts

CommandDown:
	MovePlayerBy(0, -1)
CommandUp:
	MovePlayerBy(0, 1)
CommandRight:
	MovePlayerBy(1, 0)
CommandLeft:
	MovePlayerBy(-1, 0)

CommandFoo:
	txa
	jsr	CHROUT
	PrintMessage("Valid but unimplemented command received")
	sec
	rts

CommandSelf:	
	PrintMessage("Printing character info")
	clc
	rts

Command_a:	
	PrintMessage("Aim wand")
	sec
	rts

Command_l:
	PrintMessage("Look around")
	DrawScene()
	PrintPlayer()
	clc
	rts

Command_q:	
	PrintMessage("Quitting")
	QuitGame()
	sec
	rts

Command_r:	
	PrintMessage("Resting to full HP")
	mov	#250 : PlayerHP
	sec
	rts

Command_z:	
	PrintMessage("Zap rod")
	sec
	rts

Command_upstairs:
	dec	PlayerZ
	EnterDungeon()
	sec
	rts

Command_downstairs:
	inc	PlayerZ
	EnterDungeon()
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
;	Game specific macros
;
;----------------------------------------------------------

; Prints a constant string to the message bar with -more- if needed
.macro	PrintMessage(string) {
	mov16	#Text : Message
	mov	#End - Text : MessageSize
	
	jsr	_PrintMessage
	jmp	End
Text:	.encoding "screencode_mixed"
	.text	string
	.encoding "petscii_mixed"
	
End:	
}

_more_ = "-more-"
_more:	
	.encoding "screencode_mixed"
	.text	_more_
	.encoding "petscii_mixed"

_PrintMessage:
	lda	MessageCursor
	clc
	adc	MessageSize
	cmp	#SCREENW - _more_.size()
	bcc	!+
	Copy(_more, SCREENADDR + SCREENW - _more_.size(), _more_.size())
	GetKey()
	ClearMessage()
!:	ldx	#0
!:	lda	Message:$beef, x
	sta	MessageCursor:SCREENADDR
	inc	MessageCursor
	inx
	cpx	MessageSize:#0
	bne	!-
	inc	MessageCursor
	lda	MessageCursor
	rts

.macro	ClearMessage() {
	Fill(SCREENADDR, ' ', SCREENW)
	mov	#0 : MessageCursor
}

; Prints a char at given scene coordinates
; X, Y: scene relative coordinates, A: character to print
.macro	PrintSceneTile() {
	sta	char
	mov	SceneLo, y : pos
	mov	SceneHi, y : pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
}

; Prints a char at a given dungeon position
; X, Y: dungeon relative coordinates, A: character to print
.macro	PrintDungeonTile() {
	jsr	_PrintDungeonTile
}

_PrintDungeonTile: {
	sta	char
	txa
	sec
	sbc	OffsetX
	tax
	tya
	sec
	sbc	OffsetY
	tay
	mov	SceneLo, y : pos
	mov	SceneHi, y : pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
	rts
}

; Replaces Player with underlying tile on screen
.macro	HidePlayer() {
	;ldx	PlayerX
	;ldy	PlayerY
	;RenderTile()
	lda	#'.'
	ldx	PlayerX
	ldy	PlayerY
	PrintDungeonTile()
}

; Makes Player visible
.macro	PrintPlayer() {
	ldx	PlayerX
	ldy	PlayerY
	lda	#00		; '@'
	PrintDungeonTile()
}

SceneLo:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) .byte <(SCREENADDR + SCENEOFFSET + i * SCREENW)
SceneHi:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) .byte >(SCREENADDR + SCENEOFFSET + i * SCREENW)

;----------------------------------------------------------
;
;	Called upon entering a new level
;
;----------------------------------------------------------

.macro	EnterDungeon() {
	jsr	_EnterDungeon
}

_EnterDungeon: {
	lda	#10		; Entry point
	sta	PlayerX
	sta	PlayerY
	lda	#2
	sta	OffsetX
	sta	OffsetY
	OffsetUpdated()

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
	HashA()
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

	lda	DungeonW	; For AdjustOffset()
	sec
	sbc	#SCENEW
	sta	ox1
	sta	ox2
	sta	ox3
	lda	DungeonH	; For AdjustOffset()
	sec
	sbc	#SCENEH
	sta	oy1
	sta	oy2
	sta	oy3
	
	ClearScene()
	DrawVisibleScene()
	PrintPlayer()
	rts
}

;----------------------------------------------------------
;
;	General macros & pseudocommands
;
;----------------------------------------------------------

; Prints a constant string
.macro	Print(string) {
	ldy	#0
Loop:	lda	Output,y
	beq	End
        jsr     CHROUT
	iny
	jmp	Loop
Output:	.text	string
	.byte	0
End:	
}

; Prints a char (stored in A)
.macro	PrintC(char) {
	lda	#char
        jsr     CHROUT
}

; Prints the decimal value of X
.macro	PrintX() {
	lda	#0
	jsr	$BDCD
}

; Prints the decimal value of A
.macro	PrintA() {
	tax
	PrintX()
}

; Prints a 16 bit decimal value
.macro	PrintN16(arg) {
	ldx	arg
	lda	arg + 1
	jsr	$BDCD
}

; Prints a char at a given screen position
; X, Y: screen relative coordinates, A: character to print
.macro	PrintCXY(char) {
	mov	RowsLo, y : pos
	mov	RowsHi, y : pos+1
	lda	#char
	sta	pos:SCREENADDR, x
}
.macro	PrintAXY() {
	sta	char
	mov	RowsLo, y : pos
	mov	RowsHi, y : pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
}

RowsLo:
.for(var i = SCREENH - 1 ; i != 0 ; i--) .byte <(SCREENADDR + i * SCREENW)
RowsHi:
.for(var i = SCREENH - 1 ; i != 0 ; i--) .byte >(SCREENADDR + i * SCREENW)

; Returns key in A
.macro	GetKey() {
Wait:	jsr	GETIN
	cmp	#0
	beq	Wait
}

; Fill "len" bytes of memory starting from "addr" with "byte". If "len" is 0 then fill 256 bytes.
.macro	Fill(addr, byte, len) {
	ldx	#len - 1
	lda	#byte
Loop:	sta	addr, x
	dex
	bne	Loop
	sta	addr
}

; Copy "len" bytes of memory starting from "src" to "dst". If "len" is 0 then copy 256 bytes.
.macro	Copy(src, dst, len) {
	ldx	#len - 1
Loop:	mov	src, x : dst, x
	dex
	bne	Loop
	mov	src, x : dst, x
}

.pseudocommand mov src:tar {
	lda	src
	sta	tar
}

.function _16bitnextArgument(arg) {
	.if (arg.getType() == AT_IMMEDIATE) 
		.return CmdArgument(arg.getType(), >arg.getValue())
	.return CmdArgument(arg.getType(), arg.getValue()+1)
}

.pseudocommand mov16 src:tar {
	lda	src
	sta	tar
	lda	_16bitnextArgument(src)
	sta	_16bitnextArgument(tar)
}

.pseudocommand mov24 src:tar {
	.if (src.getType() == AT_IMMEDIATE) {
		lda	src
		sta	tar
		lda #0
		sta tar.getValue()+1
		sta tar.getValue()+2
	} else {
		.error("Not implemented!")
	}
}

.pseudocommand add arg1 : arg2 : tar {
	.if (tar.getType() == AT_NONE) .eval tar=arg1
	lda arg2
	clc
	adc arg1
	sta	tar
}

.pseudocommand add16 arg1 : arg2 : tar {
	.if (tar.getType() == AT_NONE) .eval tar=arg1
	clc
	lda	arg1
	adc	arg2
	sta	tar
	.if (arg2.getType() == AT_IMMEDIATE && (>arg2.getValue()) == 0) {
		bcc skip
		inc tar.getValue()+1
	skip:
	} else {
	lda	_16bitnextArgument(arg1)
	adc	_16bitnextArgument(arg2)
	sta	_16bitnextArgument(tar)
	}
}

.pseudocommand sub arg1 : arg2 : tar {
	.if (tar.getType() == AT_NONE) .eval tar=arg1
	lda arg2
	sec
	sbc arg1
	sta	tar
}

.pseudocommand sub16 arg1 : arg2 : tar {
	.if (tar.getType() == AT_NONE) .eval tar=arg1
	sec
	lda	arg1
	sbc	arg2
	sta	tar
	.if (arg2.getType() == AT_IMMEDIATE && (>arg2.getValue()) == 0) {
		bcs skip
		dec tar.getValue()+1
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
	inc	arg.getValue() + 1
over:
}

.pseudocommand inc24 arg {
	inc16	arg
	bne	over
	inc	arg.getValue() + 2
over:
}

;----------------------------------------------------------
;
;	Random & Hash
;
;----------------------------------------------------------

.pseudocommand rnd {
	lda	$d012
	eor	$dc04
	sbc	$dc05
}

.macro	InitRnd() {
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
}

.macro	InitHash() {
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
}

.macro	HashA() {
	sta	vector
	lda	vector:Hash
}

.macro	HashM(addr) {
	lda	addr
	HashA()
}

.macro	HashAwithM(addr) {
	HashA()
	eor	addr
	HashA()
}

.macro	HashAwithX() {
	eor	Hash,x
}

.macro	HashAwithY() {
	eor	Hash,y
}

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