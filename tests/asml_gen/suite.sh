#!/bin/sh

echo "---- TESTING ASML GENERATION! ----"

echo -e "--> Simple arithmetics\n"

for f in tests/asml_gen/arithm/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -asml $f > tmp.asml
    ./tools/asml tmp.asml &> /dev/null && echo "OK" || exit 1
done

exit 0
