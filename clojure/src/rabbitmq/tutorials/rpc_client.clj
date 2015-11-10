;; note: this is example code and shouldn't be run in production as-is

(ns rabbitmq.tutorials.rpc-client
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.basic :as lb]
            [langohr.consumers :as lcons]))

(def ^{:const true} q "rpc_queue")

(defn correlation-id-equals?
  [correlation-id d]
  (= (.getCorrelationId (.getProperties d)) correlation-id))

(defrecord FibonacciClient [conn ch cbq consumer]
  clojure.lang.IFn
  (invoke [this n]
    (let [correlation-id (str (java.util.UUID/randomUUID))]
      (lb/publish ch "" q (str n) {:reply-to cbq
                                   :correlation-id correlation-id})
      (lb/consume ch cbq consumer)
      (-> (first (filter (partial correlation-id-equals? correlation-id)
                         (lcons/deliveries-seq consumer)))
          .getBody
          (String. "UTF-8")
          (read-string))))
  java.io.Closeable
  (close [this]
    (.close conn)))

(defn make-fibonacci-rpc-client
  []
  (let [conn     (lc/connect)
        ch       (lch/open conn)
        cbq      (lq/declare ch "" {:auto-delete false :exclusive true})
        consumer (lcons/create-queueing ch {})]
    (->FibonacciClient conn ch (:queue cbq) consumer)))

(defn -main
  [& args]
  (with-open [fibonacci-rpc (make-fibonacci-rpc-client)]
    (println " [x] Requesting fib(30)")
    (let [response (fibonacci-rpc 30)]
      (println (format " [.] Got %s" response)))))
