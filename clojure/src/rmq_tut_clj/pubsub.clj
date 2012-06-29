;; start the server
;; lein run -m rmq-tut-clj.pubsub server

;; send some messages
;; lein run -m rmq-tut-clj.pubsub eeny meeny miny moe

(ns rmq-tut-clj.pubsub
  (:import [com.rabbitmq.client QueueingConsumer])
  (:require [rmq-tut-clj.rmqhelpers :as rmq]))

(def exchange-name "logs")

(defn rmq-send [message]
  (println "sending" message)
  (rmq/with-channel "localhost" channel
    (do
      (.exchangeDeclare channel exchange-name "fanout")
      (.basicPublish channel exchange-name "" nil (.getBytes message)))))

;; (rmq-send)

(defn rmq-recv []
  (println "starting server")
  (rmq/with-channel "localhost" channel
    (do
      (.exchangeDeclare channel exchange-name "fanout")
      (let [queueName (-> channel .queueDeclare .getQueue)
            consumer (QueueingConsumer. channel)]
        (.queueBind channel queueName exchange-name "")
        (.basicConsume channel queueName true consumer)
        (loop [delivery (.nextDelivery consumer)]
          (println (String. (.getBody delivery)))
          (recur (.nextDelivery consumer)))))))

(defn -main [arg & messages]
  (cond
    (= arg "server") (rmq/run-as-thread rmq-recv)
    :else (rmq-send (reduce str (interpose " " (into messages [arg]))))))
