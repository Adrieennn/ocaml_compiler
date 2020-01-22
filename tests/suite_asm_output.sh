#!/bin/sh

echo "---- TESTING PROGRAMS OUTPUT WITH TOOLS/ASML! ----"

echo -e "--> Simple arithmetics\n"

for f in tests/mincaml/arithm/*.ml; do
    base=$(basename "$f")
    printf "$base:\n"
    ./$PROG $f -asml > tmp.asml && ./tools/asml tmp.asml > tools.output
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm > qemu.output
    diff ../tools.output qemu.output && printf "\e[1;32mOK -> outputs match \e[0m\n" || exit 1
    cd ..
done

exit 0
