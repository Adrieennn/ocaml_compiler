#!/bin/sh

echo "---- TESTING ASM GENERATION! ----"

echo -e "--> Simple arithmetics\n"

for f in tests/asm_gen/*.ml; do
    base=$(basename "$f")
    printf "$base:\n"
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

exit 0
