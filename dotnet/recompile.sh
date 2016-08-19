#!/bin/sh

for f in */; do \
    dotnet build $f
done
