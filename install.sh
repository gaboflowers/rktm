#!/bin/sh
DESTDIR=/usr
RKTMDIR="$DESTDIR/lib/rktm"
TARGETS="lexer.rkt parser.rkt transpiler.rkt template.c"
RKTMBIN="$DESTDIR/bin/"

if [ -d "$RKTMDIR" ]; then
    echo "$RKTMDIR already exists. Overwriting contents."
else
    mkdir -m 755 -p "$RKTMDIR" || exit 1
fi

cp $(echo "$TARGETS" | xargs) "$RKTMDIR" || exit 1
cp rktm "$RKTMBIN" && echo "Done."
