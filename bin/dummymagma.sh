#!/bin/bash

echo "Welcome to dummymagma v1.0!"

while true; do
    read -p "> " line
    echo "> $line" # This line should be deleted by comint
    case "$line" in
        "hang")
            while true ; do :; done
            ;;
        *)
            echo "Input: $line" # This line should appear
            ;;
    esac
done;
