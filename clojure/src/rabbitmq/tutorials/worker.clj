(ns rabbitmq.tutorials.worker
  (:require [langohr.core :as lc]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.basic :as lb]
            [langohr.consumers :as lcons]
            [clojure.string :as s]))

(def ^{:const true} q "task_queue")

(defn ^:private occurences-of
  [^String s ^Character c]
  (let [chars (map identity s)]
    (count (filter (fn [x] (= x c)) chars))))

(defn handle-delivery
  "Handles message delivery"
  [ch {:keys [delivery-tag]} payload]
  (let [s (String. payload "UTF-8")
        n (occurences-of s \.)]
    (println (format " [x] Received %s" s))
    (Thread/sleep ^double (* 1000 n))
    (println " [x] Done")
    (lb/ack ch delivery-tag)))


(defn -main
  [& args]
  (with-open [conn (lc/connect)]
    (let [ch      (lch/open conn)]
      (lq/declare ch q :durable true :auto-delete false)
      (lb/qos ch 1)
      (println " [*] Waiting for messages. To exit press CTRL+C")
      (lcons/blocking-subscribe ch q handle-delivery))))
