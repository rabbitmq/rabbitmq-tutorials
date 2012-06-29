;; start the server...
;; lein run -m rmq-tut-clj.hello sever

;; send a message to the server...
;; lein run -m rmq-tut "Hello World!"

(ns rmq-tut-clj.hello
  (:import [com.rabbitmq.client QueueingConsumer])
  (:require [rmq-tut-clj.rmqhelpers :as rmq]))

(def queue-name "hello")

(defn rmq-send [message]
  (rmq/with-channel "localhost" channel
    (do 
      (.queueDeclare channel queue-name false false false nil)
      (.basicPublish channel "" queue-name nil (.getBytes "Hello World!")))))

;; (rmq-send)

(defn rmq-recv []
  (println "server starting...")
  (rmq/with-channel "localhost" channel
    (do 
      (.queueDeclare channel queue-name false false false nil)
      (let [consumer (QueueingConsumer. channel)]
        (.basicConsume channel queue-name true consumer)
        (loop [delivery (.nextDelivery consumer)]
          (println (String. (.getBody delivery)))
          (recur (.nextDelivery consumer)))))))

(defn -main [arg]
  (cond
    (= arg "server") (rmq/run-as-thread rmq-recv)
    :else (rmq-send arg)))
