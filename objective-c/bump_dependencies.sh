#!/bin/bash

set -ex

rm -rf ~/Library/Caches/org.carthage.CarthageKit

for dir in `ls | grep "tutorial[1-9]"`
do
    cd $dir
    carthage update --platform iOS
    carthage copy-frameworks
    cd ..
done
