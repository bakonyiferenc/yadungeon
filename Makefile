PROJECT=yadungeon
PRG=$(PROJECT).prg
PLATFORM=C64
#PLATFORM=C128
#PLATFORM=X16

ifeq ($(PLATFORM),C64)
	RUN=x64 -autostart $(PRG)
endif
ifeq ($(PLATFORM),C128)
	RUN=x128 -autostart $(PRG)
endif
ifeq ($(PLATFORM),X16)
	RUN=x16emu -prg $(PRG) -run
endif

ALL_ASM=$(wildcard *.asm)
ALL_INC=$(wildcard *.inc)

all: $(ALL_ASM) $(ALL_INC)
	java -jar ~/KickAssembler/KickAss.jar -define $(PLATFORM) $(ALL_ASM)

clean:
	rm -f *.PRG *.prg *.sym

run: all
	$(RUN)
