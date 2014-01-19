#!/bin/bash

echo "Welcome to dummymagma v1.0!"

while true; do
    read -p "> " line
    echo "> $line" # This line should be deleted by comint
    echo "Input: $line , Output: 42" # This line should appear
done;
