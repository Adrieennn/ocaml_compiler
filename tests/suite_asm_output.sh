#!/bin/sh

echo "---- TESTING PROGRAMS OUTPUT WITH TOOLS/ASML! ----"

echo -e "\n\033[1m--> Simple arithmetics\033[0m"

for f in tests/mincaml/arithm/*.ml; do
    base=$(basename "$f")
    printf "$base:\n"
    ./$PROG $f -asml > tmp.asml && ./tools/asml tmp.asml > tools.output
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm > qemu.output
    diff ../tools.output qemu.output && \
      printf "\e[1;32mOK -> outputs match \e[0m\n" || exit 1
    cd ..
done

echo -e "\n\033[1m--> Spilling\033[0m"

for f in tests/mincaml/spilling/*.ml; do
    base=$(basename "$f")
    printf "$base:\n"
    ./$PROG $f -asml > tmp.asml && ./tools/asml tmp.asml > tools.output
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm > qemu.output
    diff ../tools.output qemu.output && \
      printf "\e[1;32mOK -> outputs match \e[0m\n" || exit 1
    cd ..
done

echo -e "\n\033[1m--> Complex recursions\033[0m"

for f in tests/mincaml/complex_recur/*.ml; do
    base=$(basename "$f")
    printf "$base:\n"
    ./$PROG $f -asml > tmp.asml && ./tools/asml tmp.asml > tools.output
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm > qemu.output
    diff ../tools.output qemu.output && \
      printf "\e[1;32mOK -> outputs match \e[0m\n" || exit 1
    cd ..
done

exit 0
