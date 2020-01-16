#!/bin/sh

echo $PROG

echo "---- TESTING ASML GENERATION! ----"

echo -e "--> Simple arithmetics\n"

for f in tests/asml_gen/arithm/*; do
    printf "$(basename "$f"):    "
    ./$PROG $f > /tmp/tmp.asml
    ./tools/asml /tmp/tmp.asml &> /dev/null && echo "OK" || echo "FAILURE"
done

