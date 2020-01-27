#!/bin/sh

echo $PROG


echo -e "\n\e[44m\033[1m---- TESTING TYPECHECKING! ----\033[0m"

echo -e "\e[4m\033[1m--> Invalid files\033[0m"

for f in tests/typechecking/invalid/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -t $f &> /dev/null && exit 1 || printf "\e[1;32mOK \e[0m\n"
done

echo -e "\n\e[4m\033[1m--> Valid files\033[0m"

for f in tests/typechecking/valid/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -t $f &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

exit 0
