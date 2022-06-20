#!/bin/sh

G=/usr/bin/gnuplot

D=./output_dir

I=$1
O=$D/$2

$G > $O <<EOF

#set terminal jpeg
set terminal png

f(x) = 1331000 + 30000 * (1 / (x - 2.45))

# plot [x=1:10] [1:10] f(x)
# plot sin(x), cos(x)
# , f(x)

plot "$I" using 1:2

EOF

mv "$I" "$D"
