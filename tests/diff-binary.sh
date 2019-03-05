#!/usr/bin/env bash

help_text=$(cat <<-END

Error! You provided less than two arguments.

Usage:

$ ./diff-binary.sh EXPECTED_BINARY_FILE_PATH ACTUAL_BINARY_FILE_PATH


This script should help when debugging the "parser" tests.
Since binary is not very readable, it can be very hard to see
what is going on if the binary encoding you provide differs from
the expected one.

If you run this script by passing in the paths of the two files,
in case the two files differ, the script will print their CBOR
encodings in JSON format, for easier inspection.


END
)

if [ -z "$2" ]
  then
    echo "$help_text"
    exit 1
fi

expected=$1
actual=$2

if diff $expected $actual; then
    echo "The two files are byte-to-byte equal, all good"
else
    printf "\n\nFound difference between the files, rendering in JSON encoding:\n"
    printf "\n\nThe expected file is encoded in this way:\n\n"
    cat $expected | dhall decode | dhall encode --json
    printf "\n\nWhile the actual one encodes like this:\n\n"
    cat $actual | dhall decode | dhall encode --json
    exit 1
fi
