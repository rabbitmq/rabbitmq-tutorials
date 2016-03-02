#!/bin/sh

sbcl --noinform --noprint <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ("amqp://")
  (with-channel ()
    (publish (exchange.default) "Hello world!" :routing-key "hello")
    (format t " [x] Sent 'Hello World!'~%")))

EOF
