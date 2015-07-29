#!/bin/sh

for f in *.cs; do \
    mcs -r:lib/bin/RabbitMQ.Client.dll $f; \
done
