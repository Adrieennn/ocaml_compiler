#!/bin/sh

echo $PROG

echo "---- TESTING TYPECHECKING! ----"

echo -e "--> Invalid files\n"

for f in tests/typechecking/invalid/*; do
    printf "$(basename "$f"):    "
    ./$PROG $f &> /dev/null && echo "FAILURE" || echo "OK"
done

echo -e "\n--> Valid files\n"

for f in tests/typechecking/valid/*; do
    printf "$(basename "$f"):    "
    ./$PROG $f &> /dev/null && echo "OK" || echo "FAILURE"
done

