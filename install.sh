#!/bin/sh
DESTDIR=/usr
RKTMDIR="$DESTDIR/lib/rktm"
TARGETS="lexer.rkt parser.rkt transpiler.rkt template.c"
BINDIR="$DESTDIR/bin"

if ! command -v racket &> /dev/null; then
    echo "Install racket first."
    exit 1
fi

if [ -d "$RKTMDIR" ]; then
    echo "$RKTMDIR already exists. Overwriting contents."
else
    mkdir -m 755 -p "$RKTMDIR" || exit 1
fi

if [ ! -d "$BINDIR" ]; then
    mkdir -m 755 -p "$BINDIR" || exit 1
fi

cp $(echo "$TARGETS" | xargs) "$RKTMDIR" || exit 1
cp rktm "$BINDIR/rktm" && echo "Done."

additionally=$(cat <<EOF

Additionally, in the BINDIR ($BINDIR), you could do:
$ mv rktm rktm.rkt           # rename script
$ raco exe -o rktm rktm.rkt  # compile a binary version
EOF
)
echo "$additionally"
