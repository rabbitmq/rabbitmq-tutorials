(ns rabbitmq.tutorials.receive
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.consumers :as lcons]))

(defn handle-delivery
  "Handles message delivery"
  [ch metadata payload]
  (println (format " [x] Received %s" (String. payload "UTF-8"))))

(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch   (lch/open conn)]
      (lq/declare ch "hello" :durable false :auto-delete false)
      (println " [*] Waiting for messages. To exit press CTRL+C")
      (lcons/blocking-subscribe ch "hello" handle-delivery :auto-ack true))))
