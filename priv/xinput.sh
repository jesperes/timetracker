#!/bin/bash

xinput "$@" &
pid=$!
# shellcheck disable=SC2034
while read -r f; do
    sleep infinity
done
kill "$pid"
