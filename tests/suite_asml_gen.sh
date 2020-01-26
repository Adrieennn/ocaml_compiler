#!/bin/sh

echo "---- TESTING ASML GENERATION! ----"

echo -e "\n\033[1m--> Simple arithmetics\033[0m"

for f in tests/mincaml/arithm/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

echo -e "\n\033[1m--> Spilling\033[0m"

for f in tests/mincaml/spilling/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

echo -e "\n\033[1m--> Complex recursions\033[0m"

for f in tests/mincaml/complex_recur/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

echo -e "\n\033[1m--> More complex arithmetics\033[0m"

for f in tests/mincaml/complex_arithm/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && printf "\e[1;32mOK \e[0m\n" || exit 1
done

exit 0
