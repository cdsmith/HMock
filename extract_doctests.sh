#!/bin/sh

CMD=doctest-extract
which $CMD > /dev/null || CMD=doctest-extract-0.1

which $CMD > /dev/null || \
    echo "Please install doctest-extract to run this script."
which $CMD > /dev/null || \
    exit 1

MODULES=$(grep -l -- '-- >>>' $(find src -name *.hs) \
             | sed 's/src\///' | sed 's/\.hs$//' | sed 's/\//./g')

$CMD --import-dir src \
     --output-dir test \
     --import-tested \
     --module-prefix DocTests \
     --library-main All \
     $MODULES
