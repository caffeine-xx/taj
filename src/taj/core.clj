(ns taj.core
  (:require [taj.mbt :as mbt])
  (:use [clj-time.core :only [now]])
  (:use [clj-time.coerce :only [to-long]])
  (:require [somnium.congomongo :as mongo]))

(defn- add-timestamp [q]
  (assoc q :ts (to-long (now))))

(defn process-quote-stream [stream]
  "Process a lazy list of quotes
    - Get a quote
    - Timestamp it
    - Save into Mongo"
  (doseq [raw-quote stream]
    (let [ts-quote  (add-timestamp raw-quote)] 
      (mongo/insert! :stream ts-quote))))

