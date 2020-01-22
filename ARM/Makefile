export PATH := /opt/gnu/arm/bin:$(PATH)

AS=arm-none-eabi-as
LD=arm-none-eabi-ld

ASM=$(wildcard *.s)
PRG=$(subst .s,.arm,${ASM})

all: ${PRG}

echo:
	echo ${ASM}
	echo ${PRG}

%.o : %.s libmincaml.S
	${AS} -mcpu=cortex-a8 -march=armv7-a -mfpu=neon -o $@ $^

%.arm : %.o
	${LD} -o $@ $^

test: ${PRG}
	@for p in ${PRG} ; do \
	  echo "*** Launching $$p"; \
	  qemu-arm ./$$p ; \
	done

