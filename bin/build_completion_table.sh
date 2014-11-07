#! /bin/bash

[ -f "magma-mode.el" ] \
    || { echo "Please start this script from the base directory of the magma-mode." ; exit 1; }



if [ $1 ]; then
    magmadir=$1
else
    echo "Usage : \"bin/build_completion_table.sh <path_to_magma.exe>\""
    echo "No path provided, looking for the magma executable"
    magmadir=$(dirname $(which magma))
    echo "Found magma in $magmadir"
fi;

magmadoc="$magmadir/doc/html"
echo "Reading data in $magmadoc"
indexfile="data/magma_symbols.txt"

tmp=$(mktemp)

cat ${magmadoc}/*.htm > $tmp

cat $tmp \
    | sed -nr "s/^.*NAME = \"([A-Z][[:alnum:]]*)\".*$/\1/p" \
    | sort > $indexfile



    
echo "Done, output is in $indexfile"

rm -f $tmp
