(ns rabbitmq.tutorials.new-task
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.basic :as lb]
            [clojure.string :as s]))

(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch      (lch/open conn)
          payload (if (empty? args)
                    "Hello, world!"
                    (s/join " " args))]
      (lq/declare ch "task_queue" {:durable true :auto-delete false})
      (lb/publish ch "" "task_queue" payload {:persistent true})
      (println (format " [x] Sent %s" payload)))))
