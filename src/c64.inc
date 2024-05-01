#importonce
#import "commodore.inc"

;----------------------------------------------------------
;
;	Machine specific
;
;----------------------------------------------------------

.const ZP_FREE0         =       $FB
.const ZP_FREE1         =       $FC
.const ZP_FREE2         =       $FD
.const ZP_FREE3         =       $FE

.const	SCREENADDR	=	$0400
.const	SCREENW		=	40
.const	SCREENH		=	25

;----------------------------------------------------------
;
;	Game specific
;
;----------------------------------------------------------

.const	SCENEOFFSET	=	SCREENW + 11
.const	SCENEW		=	29	; Size of displayed scene
.const	SCENEH		=	24
