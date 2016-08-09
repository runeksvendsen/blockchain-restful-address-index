#!/bin/bash
set -e

CONF=$1
CONFDIR="$CONF/"

cd "$CONFDIR"

BINNAME="rest-addr"
BINDIR="$(stack path --local-install-root)/bin"

BUNDLENAME="rest-addr-$CONF.keter"

cp "$BINDIR/$BINNAME" ./
strip $BINNAME

tar czf ../$BUNDLENAME $BINNAME config

echo "Created file:" >2
echo "$FILENAME"
