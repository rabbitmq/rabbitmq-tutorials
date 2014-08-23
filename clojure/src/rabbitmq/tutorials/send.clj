(ns rabbitmq.tutorials.send
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.basic :as lb]))


(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch   (lch/open conn)]
      (lq/declare ch "hello" {:durable false :auto-delete false})
      (lb/publish ch "" "hello" (.getBytes "Hello World!" "UTF-8"))
      (println " [x] Sent 'Hello World!'"))))
