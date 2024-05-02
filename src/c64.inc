.ifndef _C64_INC_
_C64_INC_ = 1

.include "commodore.inc"

;----------------------------------------------------------
;
;	Machine specific
;
;----------------------------------------------------------

ZP_FREE0         =       $FB
ZP_FREE1         =       $FC
ZP_FREE2         =       $FD
ZP_FREE3         =       $FE

SCREENADDR	=	$0400
SCREENW		=	40
SCREENH		=	25

BLACK = 0

.macro rnd
	lda	$d012
	eor	$dc04
	sbc	$dc05
.endmacro

;----------------------------------------------------------
;
;	Game specific
;
;----------------------------------------------------------

SCENEOFFSET	=	SCREENW + 11
SCENEW		=	29	; Size of displayed scene
SCENEH		=	24

.endif  ; _C64_INC_