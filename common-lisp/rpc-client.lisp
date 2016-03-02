#!/bin/sh

sbcl --noinform --noprint <<EOF

(ql:quickload :cl-bunny.examples)
(ql:quickload :nibbles)

(in-package :cl-bunny.examples)

(defun int64-to-octets(val)
  (let ((obuffer (fast-io:make-output-buffer)))
    (fast-io:write64-be val obuffer)
    (fast-io:finish-output-buffer obuffer)))

(defun start-client (n)
  (with-connection ("amqp://")
    (with-channel ()
      (let ((x (exchange.default))
            (server-queue "rpc_queue")
            (reply-queue (queue.declare :auto-delete t))
            (lock (bt:make-lock))
            (condition (bt:make-condition-variable))
            (result nil))
        (format t " [x] Requesting fib(~a)~%" n)
        (bt:with-lock-held (lock)
          (subscribe reply-queue (lambda (message)
                                    (bt:with-lock-held (lock)
                                      (setf result (nibbles:sb64ref/be (message-body message) 0))
                                      (bt:condition-notify condition))))
          (publish x
                   (int64-to-octets n)
                   :routing-key server-queue
                   :properties (list :correlation-id (format nil "~a~a~a" (random 100) (random 100) (random 100))
                                     :reply-to reply-queue))
          (bt:condition-wait condition lock)
          (format t " [.] Got ~a~%" result)
          result)))))

(start-client 0)
(start-client 1)
(start-client 22)
(start-client 33)
(start-client 44)
(start-client 55)

EOF
