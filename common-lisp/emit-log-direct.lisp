#!/bin/sh

sbcl --noinform --noprint $@ <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ()
  (with-channel ()
    (let* ((args (rest sb-ext:*posix-argv*))
           (severity (or (first args) "info"))
           (msg (format nil "~{~a~^ ~}" (rest args)))
           (x (exchange.direct "direct_logs")))
      (publish x msg :routing-key severity)
      (format t " [x] Sent '~a'~%" msg))))

EOF
