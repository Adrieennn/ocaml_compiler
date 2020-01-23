make
./mincamlc $1 -o out.s
mv out.s ARM/
cd ARM
make
qemu-arm out.arm
echo
