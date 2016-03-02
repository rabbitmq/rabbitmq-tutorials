#!/bin/sh

sbcl --noinform --noprint <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ("amqp://")
  (with-channel ()
    (let ((q (queue.declare-temp)))
      (queue.bind q "logs")
      (format t " [*] Waiting for logs. To exit press CTRL+C~%")
      (handler-case
          (progn
            (subscribe q (lambda (message)
                            (format t " [x] #~a~%" (message-body-string message)))
                       :type :sync)
            (consume))
        (sb-sys:interactive-interrupt ()
          (sb-ext:exit))))))

EOF
