#!/bin/bash

LINE_COUNT=$(find ./goal_src -name '*.gc' | xargs wc -l | tail -1 | awk -F'[^0-9]+' '{ print $2 }')

sed -i "s/.*value.*/      \"value\": ${LINE_COUNT},/g" ./docs/gh-pages-proj/src/config/progress.json
