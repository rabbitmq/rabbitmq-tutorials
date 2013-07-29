;; start multiple server server
;; lein run -m rmq-tut-clj.work-queues server
;; lein run -m rmq-tut-clj.work-queues server

;; send some messages
;; lein run -m rmq-tut-clj.work-queues ole dole doff

(ns rmq-tut-clj.work-queues
  (:import [com.rabbitmq.client MessageProperties QueueingConsumer])
  (:require [rmq-tut-clj.rmqhelpers :as rmq]))

(def queue-name "task_queue")

(defn rmq-send [message]
  (println "sending" message)
  (rmq/with-channel "localhost" channel
    (do
      ;; first true here is durable queue
      (.queueDeclare channel queue-name true false false nil)
      (.basicPublish channel "" queue-name
                     MessageProperties/PERSISTENT_TEXT_PLAIN  ;; persistent message
                     (.getBytes message)))))

;; (rmq-send)

(defn worker []  
  (println "worker starting")
  (rmq/with-channel "localhost" channel
    (do
      (.queueDeclare channel queue-name true false false nil)
      ;; dispatch fairness (don't dispatch until ack)
      ;; simple load balancing
      (.basicQos channel 1) ;; prefetch count
      (let [consumer (QueueingConsumer. channel)]
        ;; false - don't auto-ack
        (.basicConsume channel queue-name false consumer)

        (loop [delivery (.nextDelivery consumer)]

          ;; Do some work
          (let [message (String. (.getBody delivery))]
            (print "Processing '" message "'...")
            (flush)
            (doseq [ch message]
              (when (= ch \.) (Thread/sleep 1000)))
            (println "done"))
          
          (.basicAck channel (-> delivery .getEnvelope .getDeliveryTag) false)
          (recur (.nextDelivery consumer)))))))

(defn -main [arg & messages]
  (cond
    (= arg "server") (rmq/run-as-thread worker)
    :else (dotimes [i 5] (rmq-send (reduce str (interpose "." (into messages [arg (str i)])))))))

