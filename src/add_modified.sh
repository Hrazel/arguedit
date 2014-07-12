#!/usr/bin/env bash

for x in "$(git status | grep modified | awk '{print $3}')"; do git add $x; done
git status
#git status | awk '/modified/ { print $3 }' | git add
