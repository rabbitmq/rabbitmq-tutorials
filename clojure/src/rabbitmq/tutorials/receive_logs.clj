(ns rabbitmq.tutorials.receive-logs
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.exchange :as le]
            [langohr.queue :as lq]
            [langohr.basic :as lb]
            [langohr.consumers :as lcons]))

(def ^{:const true} x "logs")

(defn handle-delivery
  "Handles message delivery"
  [ch metadata payload]
  (println (format " [x] %s" (String. payload "UTF-8"))))


(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch              (lch/open conn)
          {:keys [queue]} (lq/declare ch "" {:durable true :auto-delete false})]
      (le/fanout ch x {:durable false :auto-delete false})
      (lq/bind ch queue x)
      (println " [*] Waiting for logs. To exit press CTRL+C")
      (lcons/blocking-subscribe ch queue handle-delivery))))
