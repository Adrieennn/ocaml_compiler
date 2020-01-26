#!/bin/sh

echo "---- TESTING ASM GENERATION! ----"

echo -e "\n\033[1m--> Simple arithmetics\033[0m"

for f in tests/mincaml/arithm/*.ml; do
    base=$(basename "$f")
    printf "$base:\n"
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && \
      printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

echo -e "\n\033[1m--> Closures\033[0m"

for f in tests/mincaml/closures/*; do
    printf "$(basename "$f"):\t"
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && \
      printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

echo -e "\n\033[1m--> Spilling\033[0m"

for f in tests/mincaml/spilling/*; do
    printf "$(basename "$f"):\t"
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && \
      printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

echo -e "\n\033[1m--> Complex arithmetics\033[0m"

for f in tests/mincaml/complex_arithm/*; do
    printf "$(basename "$f"):\t"
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && \
      printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

echo -e "\n\033[1m--> Complex recursions\033[0m"

for f in tests/mincaml/complex_recur/*; do
    printf "$(basename "$f"):\t"
    ./$PROG $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && \
      printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

exit 0
