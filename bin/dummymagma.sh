#!/bin/bash

echo "Welcome to dummymagma v1.0!"
echo "Current working directory: ${PWD}"
echo "(Type \"help\" for help)"

prompt="> "
debuggerOn=0;


while true; do
    IFS='' read -p "$prompt" line
    # python -c "print '> $line' + ''*(len('$line')+3)"
    #echo "> $line" # This line should be deleted by comint
    echo "$prompt $line"
    case "$line" in
        "help")
            echo "Commands:"
            echo "- debug;   : start the debugger. Namely, replace the prompt with \"debug> \" "
            echo "- error-relative;   : output an error with relative path"
            echo "- error-absolute;   : output an error with absolute path"
            echo "- hang;    : the process will hang until killed"
            echo "- help;    : display this"
            echo "- print ...;" : print the argument
            echo "- quit;    : quit the debug mode, or dummy-magma if the debugger wasn't running"
            echo "- silence; : no output"
            echo "Anything else is just reechoed."
            ;;
        "debug;")
            prompt="debug$prompt"
            debuggerOn=1
            ;;
        "error-absolute;")
            echo "In file \"${PWD}/features/testfiles/error.m\", line 2, column 4:"
            echo "Error"
            ;;
        "error-relative;")
            echo "In file \"features/testfiles/error.m\", line 1, column 4:"
            echo "Error"
            ;;
        "hang;")
            while true ; do :; done
            ;;
        "print"*";")
            eval echo ${line:6:-1}
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
