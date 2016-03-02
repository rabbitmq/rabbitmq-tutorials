#!/bin/sh

sbcl --noinform --noprint <<EOF

(ql:quickload :cl-bunny.examples)

(in-package :cl-bunny.examples)

(with-connection ()
  (with-channel ()
    (let ((q (queue.declare :name "task_queue" :durable t)))
      (format t " [*] Waiting for messages in queue 'task_queue'. To exit press CTRL+C~%")
      (qos :prefetch-count 1)
      (handler-case
          (progn
            (subscribe q (lambda (message)
                            (let ((body (message-body-string message)))
                              (format t " [x] Received '~a'~%" body)
                              ;; imitate some work
                              (sleep (count #\. body))
                              (message.ack message)
                              (format t " [x] Done~%")))
                       :type :sync)
            (consume))
        (sb-sys:interactive-interrupt ()
          (sb-ext:exit))))))

EOF
