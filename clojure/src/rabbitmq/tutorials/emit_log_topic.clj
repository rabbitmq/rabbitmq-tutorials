(ns rabbitmq.tutorials.emit-log-topic
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.exchange :as le]
            [langohr.basic :as lb]
            [clojure.string :as s]))

(def ^{:const true} x "topic_logs")

(defn -main
  [severity & args]
  (with-open [conn (lc/connect)]
    (let [ch      (lch/open conn)
          severity (or severity "anonymous.info")
          payload  (if (empty? args)
                     "Hello, world!"
                     (s/join " " args))]
      (le/topic ch x {:durable false :auto-delete false})
      (lb/publish ch x severity payload)
      (println (format " [x] Sent %s" payload)))))
