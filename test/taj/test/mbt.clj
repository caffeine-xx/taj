(ns taj.test.mbt
  (:use [taj.mbt] :reload)
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(defn test-do [f & args] 
  (def *qs* (apply (partial f *qs*) args))
  (println *qs*))

(def *qs* nil)
(def test-session
  {:host     "127.0.0.1"
   :port      5020
   :username "tu"
   :password "tp"})

(defn test-reset []
  (when *qs* (mbt-close *qs*))
  (def  *qs* (mbt-connect test-session))
  *qs*)

(defn test-split-msg []
  (let [msg "G|101=tu"
        [head data] (split-msg msg)]
    (println {:msg msg
              :head head
              :data data})))

(defn test-rekey []
  (let [h {:a 1 :b 2}
        m {:a :A :b :B}
        n (rekey h m)]
    (println n)
    (assert (= n {:A 1 :B 2}))))

;(test-rekey)
;(test-split-msg)
;(test-reset)
;(test-do mbt-ping)
;(test-do mbt-subscribe "AAPL" :level1 println)
;(test-do mbt-read)
