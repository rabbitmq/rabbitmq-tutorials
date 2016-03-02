#!/bin/sh

sbcl --noinform --noprint $@ <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ("amqp://")
  (with-channel ()
    (let ((msg (format nil "~{~a~^ ~}" (cdr sb-ext:*posix-argv*)))
          (x (exchange.fanout "logs")))
      (publish x msg)
      (format t " [x] Sent '~a'~%" msg))))

EOF
