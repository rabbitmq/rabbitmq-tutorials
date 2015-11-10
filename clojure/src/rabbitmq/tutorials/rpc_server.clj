(ns rabbitmq.tutorials.rpc-server
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.basic :as lb]
            [langohr.consumers :as lcons]))

(def ^{:const true} q "rpc_queue")

(defn fib
  [n]
  (if (zero? n)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))

(defn handle-delivery
  "Handles message delivery"
  [ch {:keys [delivery-tag reply-to correlation-id]} payload]
  (let [n (read-string (String. payload "UTF-8"))]
    (println (format " [.] fib(%s)" n))
    (let [response (fib n)]
      (lb/publish ch "" reply-to (str response) {:correlation-id correlation-id})
      (lb/ack ch delivery-tag))))

(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch (lch/open conn)]
      (lq/declare ch q {:auto-delete false})
      (lb/qos ch 1)
      (println " [x] Awaiting RPC requests")
      (lcons/blocking-subscribe ch "rpc_queue" handle-delivery))))
