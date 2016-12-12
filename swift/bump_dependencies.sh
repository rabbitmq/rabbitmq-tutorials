#!/bin/bash

set -ex

rm -rf ~/Library/Caches/org.carthage.CarthageKit

for tutdir in `ls | grep "tutorial[1-9]"`
do
    cd $tutdir
    carthage update --platform iOS
    cd ..
done
