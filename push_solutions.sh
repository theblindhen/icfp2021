#!/bin/bash
if [ -z $APIKEY ]; then
    echo "Please set your APIKEY environment variable."
    exit 1
fi
for i in {1..132}; do
    if [ -d problems/$i-solutions ]; then
        SMALLEST=$(cd problems/$i-solutions/; ls * | sort -n | head -1)
        if [ ! -f problems/$i-solutions/$SMALLEST.submitted ]; then
            echo "Submitting $i-solutions/$SMALLEST"
            curl -H "Authorization: Bearer $APIKEY" --data @problems/$i-solutions/$SMALLEST https://poses.live/api/problems/$i/solutions
            echo
            touch problems/$i-solutions/$SMALLEST.submitted
            git add problems/$i-solutions/$SMALLEST.submitted
        fi
    fi
done