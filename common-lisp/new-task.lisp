#!/bin/sh

sbcl --noinform --noprint $@ <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ()
  (with-channel ()
    (let ((x (exchange.default))
          (msg (format nil "~{~a~^ ~}" (cdr sb-ext:*posix-argv*))))
      (publish x msg :routing-key "task_queue"
                     :properties '(:persistent t))
      (format t " [x] Sent '~a'~%" msg))))

EOF
