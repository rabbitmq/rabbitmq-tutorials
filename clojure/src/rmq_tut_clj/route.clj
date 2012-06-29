;; start the server
;; lein run -m rmq-tut-clj.route server err warn

;; send some messages
;; lein run -m rmq-tut-clj.route warn kinke lane koff
;; lein run -m rmq-tut-clj.route info koffe lane binke bane

(ns rmq-tut-clj.route
  (:import [com.rabbitmq.client QueueingConsumer])
  (:require [rmq-tut-clj.rmqhelpers :as rmq]))

(def exchange-name "direct_logs")

(defn rmq-send [message severity]
  (println "sending" message "[" severity "]")
  (rmq/with-channel "localhost" channel
    (do
      (.exchangeDeclare channel exchange-name "direct")
      ;; severity is really routing key
      (.basicPublish channel exchange-name severity nil (.getBytes message)))))

;; (rmq-send)

(defn rmq-recv [severities]
  (println "starting server")
  (rmq/with-channel "localhost" channel
    (do
      (.exchangeDeclare channel exchange-name "direct")
      (let [queueName (-> channel .queueDeclare .getQueue)
            consumer (QueueingConsumer. channel)]
        (doseq [s severities]
          (do (println s)
              (.queueBind channel queueName exchange-name s)))
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
