(ns rabbitmq.tutorials.emit-log
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.exchange :as le]
            [langohr.basic :as lb]
            [clojure.string :as s]))

(def ^{:const true} x "logs")

(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch      (lch/open conn)
          payload (if (empty? args)
                    "Hello, world!"
                    (s/join " " args))]
      (le/fanout ch x :durable false :auto-delete false)
      (lb/publish ch x "" payload)
      (println (format " [x] Sent %s" payload)))))
