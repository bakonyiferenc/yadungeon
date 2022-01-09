#importif	C64	"c64.inc"
#importif	C128	"c128.inc"
#importif	X16	"x16.inc"

BasicUpstart2(Start)

//----------------------------------------------------------
//
//	Yet Another Dungeon
//
//----------------------------------------------------------

Start:	Init()
MainLoop:			// <- self modifying
	QuickStats()
//	DrawScene()
	PlayersTurn()
	MonstersTurn()
	IsPlayerAlive()
	inc24	Turn
	jmp	MainLoop

//----------------------------------------------------------
//
//	Initialize
//
//----------------------------------------------------------

.macro	Init() {
	lda	#BLACK
	//sta	$d020
	//sta	$d021
	Print(@"\$0e\$9b\$93Welcome to YAD!\nPress any key!\n")	// Lowercase, lightgray on black, clear screen
	GetKey()
	PrintC($93)		// clr
	NewGame()
}

//----------------------------------------------------------
//
//	Start a new game
//
//----------------------------------------------------------

.macro	NewGame() {
	InitRnd()
	InitHash()

	ldx	#0
	txa
Loop:	sta	Monster, x
	sta	MonsterX, x
	sta	MonsterY, x
	sta	MonsterZ, x
	sta	MonsterHP, x
	sta	MonsterState1, x
	sta	MonsterState2, x
	inx
	bne	Loop

	lda	#1		// Reset turn counter
	sta	Turn
	lda	#0
	sta	Turn+1
	sta	Turn+2

	lda	#100		// Dummy character creation
	sta	PlayerHP

	lda	#1		// Start from City1
	sta	PlayerZ
	
	EnterDungeon()
}

//----------------------------------------------------------
//
//	Player's turn
//
//----------------------------------------------------------

.macro	PlayersTurn() {
Loop:	GetKey()
	jsr	ProcessCommand
	bcc	Loop		// Loop until a turn has passed
}

//----------------------------------------------------------
//
//	Monsters are next
//
//----------------------------------------------------------

.macro	MonstersTurn() {
	rnd
	and	#7
	bne	End
	Print(@"\$13Monsters hit you! ")
	dec	PlayerHP
End:
}

//----------------------------------------------------------
//
//	Is player still alive?
//
//----------------------------------------------------------

.macro	IsPlayerAlive() {
	lda	PlayerHP
	bne	Alive
Dead:	Print(@"\$13You died! ")
	QuitGame()
Alive:
}

//----------------------------------------------------------
//
//	Quit game
//
//----------------------------------------------------------

.macro	QuitGame() {
	lda	#RTS
	sta	MainLoop
}

//----------------------------------------------------------
//
//	Print quickstats
//
//----------------------------------------------------------

.macro	QuickStats() {
	PrintC($13)		// home
	PrintC($11)		// down
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

//----------------------------------------------------------
//
//	Draw scene around player
//
//----------------------------------------------------------

// Draws the visible part of scene.
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

// Must be called when Offset{XY} was changed.
.macro	OffsetUpdated() {
	lda	#<(SCREENADDR + SCENEOFFSET)
	sec
	sbc	OffsetX
	sta	_screen0

	lda	#>(SCREENADDR + SCENEOFFSET)
	sbc	#0
	sta	_screen1

	lda	#SCENEW
	clc
	adc	OffsetX
	sta	_x

	lda	#SCENEH
	clc
	adc	OffsetY
	sta	_y

	lda	OffsetX
	sta	_ox

	lda	OffsetY
	sta	_oy
}

// Must be called when Player{XY} was changed. Scrolls the scene when needed.
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
AdjY:	lda	PlayerY
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
	beq	AdjY		// already at the far end, skip scroll
	clc
	adc	#SCENEW * 1 / 4
	cmp	ox2:#0
	bcc	!+
	lda	ox3:#0
!:	jmp	ScrollLeft	// What about double change (Y)?
AdjLeft:
	lda	OffsetX
	beq	AdjY		// already at the far end, skip scroll
	sec
	sbc	#SCENEW * 1 / 4
	bcs	!+
	lda	#0
!:	jmp	ScrollRight	// What about double change (Y)?
AdjUp:
	lda	OffsetY
	cmp	oy1:#0
	beq	AdjEnd		// already at the far end, skip scroll
	clc
	adc	#SCENEH * 1 / 4
	cmp	oy2:#0
	bcc	!+
	lda	oy3:#0
!:	sta	OffsetY
	jmp	ScrollDown	// What about double change (Y)?
AdjDown:
	lda	OffsetY
	beq	AdjEnd		// already at the far end, skip scroll
	sec
	sbc	#SCENEH * 1 / 4
	bcs	!+
	lda	#0
!:	sta	OffsetY
	jmp	ScrollUp	// What about double change (Y)?

ScrollRight: {
	ldx	OffsetX		// X: old OffsetX
	sta	OffsetX		// A: new OffsetX
	stx	x
	sec
	sbc	x:#0
	clc
	adc	#SCENEW - 1
	tax			// SCENEW - (X - A) - 1
	ldy	#SCENEW - 1
MoveColumn:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) {
	lda	SCREENADDR + SCENEOFFSET + i * SCREENW, x
	sta	SCREENADDR + SCENEOFFSET + i * SCREENW, y
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
	ldx	OffsetX		// X: old OffsetX
	sta	OffsetX		// A: new OffsetX
	stx	x
	sec
	sbc	x:#0
	clc
	adc	#-SCENEW
	tax			// -SCENEW + (A - X)
	ldy	#-SCENEW
MoveColumn:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) {
	lda	SCREENADDR + SCENEOFFSET + i * SCREENW + SCENEW - 256, x
	sta	SCREENADDR + SCENEOFFSET + i * SCREENW + SCENEW - 256, y
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

ScrollDown:
ScrollUp:
	OffsetUpdated()
	DrawScene()
	rts
Scrolled:
	OffsetUpdated()
	rts

// Draws the whole scene, visible or not.
.macro	DrawScene() {
	lda	#JSR_ABS
	sta	_tile
	lda	#<_RenderTile
	sta	_tile+1
	lda	#>_RenderTile
	sta	_tile+2
	jsr	_DrawScene
}

// Clears the whole scene
.macro	ClearScene() {
	lda	#NOP
	sta	_tile
	lda	#LDA_IMM
	sta	_tile+1
	lda	#' '
	sta	_tile+2
	jsr	_DrawScene
}

_DrawScene:
	lda	_screen0:#0
	sta	screen
	lda	_screen1:#0
	sta	screen+1

	ldy	_y:#0
y:	dey
	ldx	_x:#0
x:	dex
_tile:	RenderTile()
	sta	screen:SCREENADDR, x
	cpx	_ox:#0
	bne	x
	clc
	lda	screen
	adc	#SCREENW
	sta	screen
	bcc	!+
	inc	screen+1
!:	cpy	_oy:#0
	bne	y
	rts

//----------------------------------------------------------
//
//	Render a tile
//	Input: X, Y: dungeon coordinates (0-255)
//	Returns a char in A
//
//----------------------------------------------------------

.macro	RenderTile() {
	jsr	_RenderTile
}

_RenderTile:
	RenderBorder()

	cpx	#40		// Test tunnels
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
	lda	#'#'		// Fall back to default rock
	rts

// Impenetrable border around each landscape/dungeon
.macro	RenderBorder() {
	txa
	beq	Border
	cmp	DungeonMaxX
	bcs	Border
	tya
	beq	Border
	cmp	DungeonMaxY
	bcc	End
Border:	lda	#'%'		// Fence
	rts
End:
}

// A simple parametric room
.macro	RenderRoom(xsize, ysize, chance) {
	txa
	and	#-xsize	// block size X: $f0 = 16, $f8 = 8, $fc = 4, etc
	HashA()
	sta	xc

	tya
	and	#-ysize	// block size Y: $f0 = 16, $f8 = 8, $fc = 4, etc
	eor	xc:#0
	HashAwithM(PlayerZ)

	cmp	#256 * chance	// block chance: $40:$100 = 1:4
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
	lda	#'.'		// Floor
	rts
End:
}	
	
//----------------------------------------------------------
//
//	Process a command (stored in A)
//	Returns: C <- player completed a turn
//
//----------------------------------------------------------

ProcessCommand:
	cmp	#$11	// cursor down
	bne	!+
	MovePlayerBy(0, -1)
!:	cmp	#$91	// cursor up
	bne	!+
	MovePlayerBy(0, 1)
!:	cmp	#$1d	// cursor right
	bne	!+
	MovePlayerBy(1, 0)
!:	cmp	#$9d	// cursor left
	bne	!+
	MovePlayerBy(-1, 0)
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
	
//----------------------------------------------------------
//
//	Player commands
//
//----------------------------------------------------------

.macro	MovePlayerBy(x, y) {
	ldx	PlayerX
	ldy	PlayerY
.if (x == -1)	dex
.if (x ==  1)	inx
.if (y == -1)	dey
.if (y ==  1)	iny
	RenderTile()
	cmp	#'.'
	bne	End
//.if (x != 0)	stx	PlayerX
//.if (y != 0)	sty	PlayerY
	HidePlayer()
.if (x == -1)	dec	PlayerX
.if (x ==  1)	inc	PlayerX
.if (y == -1)	dec	PlayerY
.if (y ==  1)	inc	PlayerY
	AdjustOffset()
	DrawVisibleScene()
	PrintPlayer()
	sec
End:	rts
}

IllegalCommand:
	txa
	jsr	CHROUT
	Print(@"\$13Illegal command received")
	clc
	rts

CommandFoo:
	txa
	jsr	CHROUT
	Print(@"\$13Valid but unimplemented command received\n")
	sec
	rts

CommandSelf:	
	Print(@"\$13'@' command received, printing character info\n")
	clc
	rts

Command_a:	
	Print(@"\$13'a' command received, aim wand\n")
	sec
	rts

Command_l:
	Print(@"\$13Look around\n")
	DrawScene()
	PrintPlayer()
	clc
	rts

Command_q:	
	Print(@"\$13'q' command received, quitting\n")
	QuitGame()
	sec
	rts

Command_r:	
	Print(@"\$13Resting to full HP\n")
	lda	#250
	sta	PlayerHP
	sec
	rts

Command_z:	
	Print(@"\$13'z' command received, zap rod\n")
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
.word	CommandSelf	// @
.word	Command_a	// a
.fillword	'l'-'b', CommandFoo
.word	Command_l
.fillword	'q'-'m', CommandFoo
.word	Command_q
.word	Command_r
.fillword	'z'-'s', CommandFoo
.word	Command_z	// z


//----------------------------------------------------------
//
//	Game specific macros
//
//----------------------------------------------------------

// Prints a char at given scene coordinates
// X, Y: scene relative coordinates, A: character to print
.macro	PrintSceneTile() {
	sta	char
	lda	SceneLo, y
	sta	pos
	lda	SceneHi, y
	sta	pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
}

// Prints a char at a given dungeon position
// X, Y: dungeon relative coordinates, A: character to print
.macro	PrintDungeonTile() {
	sta	char
	txa
	sec
	sbc	OffsetX
	tax
	tya
	sec
	sbc	OffsetY
	tay
	lda	SceneLo, y
	sta	pos
	lda	SceneHi, y
	sta	pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
}

.macro	HidePlayer() {
	ldx	PlayerX
	ldy	PlayerY
	RenderTile()
	ldx	PlayerX
	ldy	PlayerY
	PrintDungeonTile()
}

.macro	PrintPlayer() {
	ldx	PlayerX
	ldy	PlayerY
	lda	#00		// '@'
	PrintDungeonTile()
}

SceneLo:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) .byte <(SCREENADDR + SCENEOFFSET + i * SCREENW)
SceneHi:
.for(var i = SCENEH - 1 ; i >= 0 ; i--) .byte >(SCREENADDR + SCENEOFFSET + i * SCREENW)

//----------------------------------------------------------
//
//	Called upon entering a new level
//
//----------------------------------------------------------

.macro	EnterDungeon() {
	jsr	_EnterDungeon
	ClearScene()
	DrawVisibleScene()
	PrintPlayer()
}

_EnterDungeon:
	lda	#10		// Entry point
	sta	PlayerX
	sta	PlayerY
	lda	#2
	sta	OffsetX
	sta	OffsetY
	OffsetUpdated()

	lda	PlayerZ
	bne	!+		// Wilderness has a fixed size
	lda	#$ff
	sta	DungeonW
	sta	DungeonH
	lda	#$fe
	sta	DungeonMaxX
	sta	DungeonMaxY
	jmp	!++
!:				// Calculate dungeon size
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

!:	lda	DungeonW	// For AdjustOffset()
	sec
	sbc	#SCENEW
	sta	ox1
	sta	ox2
	sta	ox3
	lda	DungeonH	// For AdjustOffset()
	sec
	sbc	#SCENEH
	sta	oy1
	sta	oy2
	sta	oy3
	rts

//----------------------------------------------------------
//
//	General macros & pseudocommands
//
//----------------------------------------------------------

.macro	Print(string) {
	ldy	#0
Loop:	lda	Output,y
	beq	End
        jsr     CHROUT
	iny
	jmp	Loop
Output:	.encoding "petscii_mixed"
	.text	string
	.byte	0
End:	
}

// Prints a char (stored in A)
.macro	PrintC(char) {
	lda	#char
        jsr     CHROUT
}

// Prints the decimal value of X
.macro	PrintX() {
	lda	#0
	jsr	$BDCD
}

// Prints the decimal value of A
.macro	PrintA() {
	tax
	PrintX()
}

// Prints the 16 bit decimal value
.macro	PrintN16(arg) {
	ldx	arg
	lda	arg + 1
	jsr	$BDCD
}

// Prints a char at a given screen position
// X, Y: screen relative coordinates, A: character to print
.macro	PrintCXY(char) {
	lda	RowsLo, y
	sta	pos
	lda	RowsHi, y
	sta	pos+1
	lda	#char
	sta	pos:SCREENADDR, x
}
.macro	PrintAXY() {
	sta	char
	lda	RowsLo, y
	sta	pos
	lda	RowsHi, y
	sta	pos+1
	lda	char:#0
	sta	pos:SCREENADDR, x
}

RowsLo:
.for(var i = SCREENH - 1 ; i != 0 ; i--) .byte <(SCREENADDR + i * SCREENW)
RowsHi:
.for(var i = SCREENH - 1 ; i != 0 ; i--) .byte >(SCREENADDR + i * SCREENW)

// Returns key in A
.macro	GetKey() {
Wait:	jsr	GETIN
	cmp	#0
	beq	Wait
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

//----------------------------------------------------------
//
//	Random & Hash
//
//----------------------------------------------------------

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
	beq	noEor	// if the input was $80, skip the EOR
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

//----------------------------------------------------------
//
//	Game date to save
//
//----------------------------------------------------------

.align	256
Monster:	.fill 256, 0
MonsterX:	.fill 256, 0
MonsterY:	.fill 256, 0
MonsterZ:	.fill 256, 0	// Current dungeon: 0 = wilderness, 1-7 = cities, 8-255 = dungeons
MonsterHP:	.fill 256, 0
MonsterState1:	.fill 256, 0	// aggroed, sleeping, confused, stunned, feared, afraid, blind, deaf
MonsterState2:	.fill 256, 0	// poisoned, bleeding, fast/slow/paralyzed, drugged, invulnerable

Rnd1:		.fill 256, 0
Rnd2:		.fill 256, 0
Hash:		.fill 256, 0
Seed:		.word	0

PlayerX:	.byte	0
PlayerY:	.byte	0
PlayerZ:	.byte	0
PlayerHP:	.byte	0
PlayerState1:	.byte	0
PlayerState2:	.byte	0

Turn:		.byte	0, 0, 0
OffsetX:	.byte	0	// To center the dungeon around the player
OffsetY:	.byte	0

//----------------------------------------------------------
//
//	Cached data (not to save)
//
//----------------------------------------------------------

DungeonW:	.byte	0	// Size of current dungeon
DungeonH:	.byte	0
DungeonMaxX:	.byte	0
DungeonMaxY:	.byte	0
