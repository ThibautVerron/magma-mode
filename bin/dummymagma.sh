#!/bin/bash

echo "Welcome to dummymagma v1.0!"
echo "(Type \"help\" for help)"

while true; do
    read -p "> " line
    echo "> $line" # This line should be deleted by comint
    case "$line" in
        "help")
            echo "Commands:"
            echo "- error: output an error"
            echo "- hang: the process will hang until killed"
            echo "- help: display this"
            echo "Anything else is just reechoed."
            ;;
        "error")
            echo "In file file.m, line 1, column 1:"
            echo "Error"
            ;;
        "hang")
            while true ; do :; done
            ;;
        "silence;")
            ;;
        *)
            echo "Input: $line" # This line should appear
            ;;
    esac
done;
