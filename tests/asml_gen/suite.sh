#!/bin/sh

echo "---- TESTING ASML GENERATION! ----"

echo -e "--> Simple arithmetics\n"

for f in tests/asml_gen/arithm/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

echo -e "--> Spilling\n"

for f in tests/asml_gen/spilling/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

exit 0
