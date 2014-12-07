#!/bin/bash

echo "Welcome to dummymagma v1.0!"
echo "(Type \"help\" for help)"

prompt="> "
debuggerOn=0;

while true; do
    IFS='' read -p "$prompt" line
    echo "> $line" # This line should be deleted by comint
    case "$line" in
        "help")
            echo "Commands:"
            echo "- debug;   : start the debugger. Namely, replace the prompt with \"debug> \" "
            echo "- error;   : output an error"
            echo "- hang;    : the process will hang until killed"
            echo "- help;    : display this"
            echo "- quit;    : quit the debug mode, or dummy-magma if the debugger wasn't running"
            echo "- silence; : no output"
            echo "Anything else is just reechoed."
            ;;
        "debug;")
            prompt="debug$prompt"
            debuggerOn=1
            ;;
        "error;")
            echo "In file file.m, line 1, column 1:"
            echo "Error"
            ;;
        "hang;")
            while true ; do :; done
            ;;
        "quit;"|"exit;")
            if [ $debuggerOn -eq 1 ]; then
                debuggerOn=0;
                prompt="> ";
            else
                break;
            fi
            ;;
        "silence;")
            ;;
        *)
            echo "Input: $line" # This line should appear
            ;;
    esac
done;
