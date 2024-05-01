.ifndef _C128_INC_
_C128_INC_ = 1

.include "commodore.inc"

.macro BasicUpstart2(address) {
    .pc = $1c01 "C128 Basic"
    .word upstartEnd  ; link address
    .word 10   ; line num
    .byte $9e  ; sys
    .text toIntString(address)
    .byte 0
upstartEnd:
    .word 0  ; empty link signals the end of the program
    .pc = $1c0e "Basic End"
}

.const	SCREENW	=	80
.const	SCREENH	=	25

.endif  ; _C128_INC_