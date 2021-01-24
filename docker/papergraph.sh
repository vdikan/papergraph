#!/bin/bash

while getopts "rs" flag
do
    case "${flag}" in
        r) /bin/papergraph ;;
        s) cp /src/papergraph/app/papergraphrc.sample . ;;
    esac
done
