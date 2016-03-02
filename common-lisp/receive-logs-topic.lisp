#!/bin/sh

sbcl --noinform --noprint $@ <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(if-let ((args (rest sb-ext:*posix-argv*)))
  (with-connection ()
    (with-channel ()
      (let ((q (queue.declare :auto-delete t))
            (x (exchange.topic "topic_logs")))
        (loop for severity in args do
                 (queue.bind q x :routing-key severity))
        (format t " [*] Waiting for logs. To exit press CTRL+C~%")
        (handler-case
            (progn
              (subscribe q (lambda (message)
                              (format t " [x] #~a:~a~%" (message-routing-key message) (message-body-string message)))
                         :type :sync)
              (consume))
          (sb-sys:interactive-interrupt ()
            (sb-ext:exit))))))
  (format t "Usage: $0 [binding key]~%"))

EOF
