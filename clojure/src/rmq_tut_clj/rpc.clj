;; run the server
;; lein run -m rmq-tut-clj.rpc server

;; fire off some RPC calls
;; lein run -m rmq-tut-clk.rpc 12

(ns rmq-tut-clj.rpc
  (:import [com.rabbitmq.client QueueingConsumer AMQP$BasicProperties$Builder])
  (:require [rmq-tut-clj.rmqhelpers :as rmq]))

(def request-queue-name "rpc_queue")

(defn rmq-call-rpc [message]
  (rmq/with-channel "localhost" channel
    (let [reply-queue-name (-> channel .queueDeclare .getQueue)
          consumer (QueueingConsumer. channel)
          corr-id (-> (java.util.UUID/randomUUID) .toString)
          props (-> (AMQP$BasicProperties$Builder.)
                    (.correlationId corr-id)
                    (.replyTo reply-queue-name)
                    .build)]
      (.basicConsume channel reply-queue-name true consumer)
      (.basicPublish channel "" request-queue-name props (.getBytes message))
      (println "sending" message)
      (loop [delivery (.nextDelivery consumer)]
        (if (-> delivery .getProperties .getCorrelationId (.equals corr-id))
          (String. (.getBody delivery))
          (recur (.nextDelivery consumer)))))))

(defn fib [n]
  (loop [back1 1, back2 0, n n]
    (cond
      (= n 0) 0
      (= n 1) (+ back1 back2)
      :else (recur back2 (+ back1 back2) (- n 1)))))

(defn rmq-rpc-server []
  (println "starting server")
  (rmq/with-channel "localhost" channel
    (do
      (.queueDeclare channel request-queue-name false false false nil)
      (.basicQos channel 1)
      (let [consumer (QueueingConsumer. channel)]
        (.basicConsume channel request-queue-name false consumer)
        (loop [delivery (.nextDelivery consumer)]
          (let [props (.getProperties delivery)
                reply-props (-> (AMQP$BasicProperties$Builder.)
                                (.correlationId (.getCorrelationId props))
                                .build)
                message (String. (.getBody delivery))
                n (Integer/parseInt message)
                response (str (fib n))]
            (println "received" message)
            (.basicPublish channel "" (.getReplyTo props) reply-props (.getBytes response))
            (.basicAck channel (-> delivery .getEnvelope .getDeliveryTag) false)
            (recur (.nextDelivery consumer))))))))

(defn -main [arg]
  (cond
    (= arg "server") (rmq/run-as-thread rmq-rpc-server)
    :else (rmq-call-rpc arg)))
