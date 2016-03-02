#!/bin/sh

sbcl --noinform --noprint <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ("amqp://")
  (with-channel ()
    (let ((q (queue.declare :name "hello")))
      (format t " [*] Waiting for messages in queue 'hello'. To exit press CTRL+C~%")
      (handler-case
          (progn
            (subscribe q (lambda (message)
                            (format t " [x] Received ~a~%" (message-body-string message)))
                       :type :sync
                       :no-ack t)
            (consume))
        (sb-sys:interactive-interrupt ()
          (sb-ext:exit))))))

EOF
