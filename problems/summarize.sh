#!/bin/bash
for pr in `ls -1 *\.problem | sort -n`; do
    echo $pr
    echo '  Hole #vertices: ' `cat $pr | jq '.hole | length'`
    echo '  Figure #vertices: ' `cat $pr | jq '.figure.vertices | length'`
    echo '  Figure #edges: ' `cat $pr | jq '.figure.edges | length'`
    echo '  Epsilon: ' `cat $pr | jq '.epsilon'`
done