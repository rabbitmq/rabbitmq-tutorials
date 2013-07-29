;; start some servers
;; lein run -m rmq-tut-clj.topic server *.orange.*
;; lein run -m rmq-tut-clj.topic server *.*.rabbit lazy.#

;; send some messages
;; lein run -m rmq-tut-clj.topic quick.orange.rabbit test
;; lein run -m rmq-tut-clj.topic lazy.orange.elephant test
;; lein run -m rmq-tut-clj.topic quick.orange.fox test
;; lein run -m rmq-tut-clj.topic lazy.brown.fox test
;; lein run -m rmq-tut-clj.topic lazy.pink.rabbit test
;; lein run -m rmq-tut-clj.topic quick.orange.male.rabbit test
;; lein run -m rmq-tut-clj.topic lazy.orange.male.rabbit test

(ns rmq-tut-clj.topic
  (:import [com.rabbitmq.client QueueingConsumer])
  (:require [rmq-tut-clj.rmqhelpers :as rmq]))

(def exchange-name "topic_logs")

(defn rmq-send [message key]
  (println "sending" message "[" key "]")
  (rmq/with-channel "localhost" channel
    (do
      (.exchangeDeclare channel exchange-name "topic")
      (.basicPublish channel exchange-name key nil (.getBytes message)))))

;; (rmq-send)

(defn rmq-recv [keys]
  (println "starting server")
  (rmq/with-channel "localhost" channel
    (do
      (.exchangeDeclare channel exchange-name "topic")
      (let [queueName (-> channel .queueDeclare .getQueue)
            consumer (QueueingConsumer. channel)]
        (doseq [k keys]
          (do (println k)
              (.queueBind channel queueName exchange-name k)))
        (.basicConsume channel queueName true consumer)
        (loop [delivery (.nextDelivery consumer)]
          (println (String. (.getBody delivery))
                   "["
                   (String. (-> delivery .getEnvelope .getRoutingKey))
                   "]")
          (recur (.nextDelivery consumer)))))))

(defn -main [arg & messages]
  (cond
    (= arg "server") (rmq/run-as-thread rmq-recv messages)
    :else (rmq-send (reduce str messages) arg)))
