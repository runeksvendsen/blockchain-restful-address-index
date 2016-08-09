#!/bin/bash

BUN1=$(./mkKeter.sh live)
BUN2=$(./mkKeter.sh test)

cp $BUN1 $BUN2 /opt/keter/incoming/
