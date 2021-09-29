#!/bin/sh

for f in */; do \
    dotnet restore $f
    dotnet build $f
done
