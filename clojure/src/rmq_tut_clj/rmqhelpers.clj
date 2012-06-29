(ns rmq-tut-clj.rmqhelpers
  (:import [com.rabbitmq.client ConnectionFactory Connection Channel]))

(defmacro with-channel [host-name channel-name body]
  `(let [factory# (ConnectionFactory.)]
     (.setHost factory# ~host-name)
     (with-open [connection# (.newConnection factory#)
                 ~channel-name (.createChannel connection#)]
       ~body)))

(defn run-as-thread [f & args]
  (-> (reify Runnable (run [this] (apply f args))) Thread. .start))
