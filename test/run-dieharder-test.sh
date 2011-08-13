#!/bin/sh

which dieharder > /dev/null || { echo "dieharder is not found. Aborting"; exit 1; }

ghc -fforce-recomp -O2 diehard-source
( date
  ./diehard-source | dieharder -a "$@" -g 200
  date
) | tee diehard.log
