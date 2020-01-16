#!/bin/sh

echo $PROG

echo "---- TESTING TYPECHECKING! ----"

echo -e "--> Invalid files\n"

for f in tests/typechecking/invalid/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -t $f &> /dev/null && exit 1 || echo "OK"
done

echo -e "\n--> Valid files\n"

for f in tests/typechecking/valid/*; do
    printf "$(basename "$f"):\t"
    ./$PROG -t $f &> /dev/null && echo "OK" || exit 1
done

exit 0
