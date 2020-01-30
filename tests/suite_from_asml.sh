#!/bin/sh

echo -e "\n\e[44m\033[1m---- TESTING COMPILING FROM ASML! ----\033[0m"


echo -e "\n\e[4m\033[1m--> All asml files\033[0m"

for f in asml/*.asml; do
    base=$(basename "$f")
    printf "$base:\t"
    ./$PROG -from-asml $f -o ARM/$base.s
    cd ARM && make && qemu-arm $base.arm &>/dev/null && \
      printf "\e[1;32mOK \e[0m\n" || exit 1
    cd ..
done

exit 0
