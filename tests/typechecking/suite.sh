#!/bin/sh

echo $PROG

echo "---- TESTING TYPECHECKING! ----"

echo -e "--> Invalid files\n"

for f in tests/typechecking/invalid/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -t $f &> /dev/null && exit 1 || printf "\e[1;32mOK \e[0m\n"
done

echo -e "\n--> Valid files\n"

for f in tests/typechecking/valid/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -t $f &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

exit 0
