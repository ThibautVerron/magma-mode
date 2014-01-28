#! /bin/bash

[ -f "magma-mode.el" ] \
    || { echo "Please start this script from the base directory of the magma-mode." ; exit 1; }



if [ $1 ]; then
    magmadir=$1
else
    echo "Usage : \"bin/build_completion_table.sh <path_to_magma.exe>\""
    echo "No path provided, using which to guess one..."
    magmadir=$(dirname $(which magma))
    echo "Guessed $magmadir"
fi;

magmadoc="$magmadir/doc/html"
indexfile="data/magma_symbols.txt"

tmp=$(mktemp)

cat ${magmadoc}/*.htm > $tmp

cat $tmp \
    | grep "NAME" \
    | sed -r "s/<[^>]*>//g" \
    | grep ":" \
    | sed -r "s/^([[:alnum:]]+)\(.*$/\1/" \
    | sed -r "s/^([[:alnum:]]+)&lt.*$/\1/" \
    | grep -v ":" \
    | sort  | uniq > $indexfile

echo "Finished preparing the index file, output is in $indexfile"

rm -f $tmp
