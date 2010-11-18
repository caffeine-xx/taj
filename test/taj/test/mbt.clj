(ns taj.test.mbt
  (:use [taj.mbt] :reload)
  (:use [clojure.test]
        [clojure.contrib.server-socket :only [create-server close-server]])
  (:require (clojure [string :as string])))

;;; utilities

(def conn (promise))  ; mock qs server
(def ss   nil) 

(def mbt  (atom nil)) ; session

(def conn-data
  {:host     "127.0.0.1"
   :port      5020
   :username "tu"
   :password "tp"})

(defn mk-server [done?]
  "Creates a server socket, and promises a connection"
  (def conn (promise))
  (letfn [(runf [in out] 
            (deliver conn {:in (mk-reader in) :out (mk-writer out)})
            (println "done:" (deref done?)))]
    (def ss (create-server (:port conn-data) runf))))

(defn mk-mbt-conn [f]
  (let [done? (promise)]
    (mk-server done?)
    (reset! mbt (mbt-connect conn-data))
    (f)
    (deliver done? true)
    (swap! mbt mbt-close)
    (close-server ss)))

(defn sock-read []
  (println "sock-read conn: " (:in @conn))
  (let [s (.readLine (:in @conn))]
    (println "sock-read:" s)
    s))

(defn sock-write [s]
  (.println (:out @conn) s)
  (println "sock-write:" s))

;;; tests

(deftest test-split-msg 
  (is (= (split-msg "G|101=tu;103=tp\n") ["G" {"101" "tu" "103" "tp"}]))
  (is (= (split-msg "A|B=1;C=2\n") ["A" {"B" "1" "C" "2"}]))
  (is (= (split-msg "A") ["A" nil])))

(deftest test-rekey 
  (let [h {:a 1 :b 2}
        m {:a :A :b :B}
        n (rekey h m)]
    (is (= n {:A 1 :B 2}))))

(deftest test-connect 
  (println "test-connect")
  (mk-mbt-conn 
    #(is (mbt-open? @mbt) "Socket connected")))

(deftest test-mbt-ping 
  (println "test-mbt-ping")
  (mk-mbt-conn 
    (fn [] 
      (swap! mbt mbt-ping)
      (is (= "9" (sock-read))))))


(deftest test-login-success
  (println "test-login-success")
  (mk-mbt-conn
    (fn []
      (swap! mbt mbt-login)
      (is (= "L|100=tu;101=tp" (sock-read)) "Login message")
      (sock-write "G|100=tu") 
      (swap! mbt mbt-read)
      (is (= :alive (:status @mbt)) "Good Login")
    )))

(deftest test-login-denied
  (println "test-login-denied")
  (mk-mbt-conn
    (fn []
      (swap! mbt mbt-login)
      (is (= "L|100=tu;101=tp" (sock-read)) "Login message")
      (sock-write "D|100=tu;103=Failz") 
      (swap! mbt mbt-read)
      (is (= :denied (:status @mbt)) "Login denied")
    )))


;(def done (promise))
;(mk-server done)
;(sock-read)

;(test-login-success)
;;; runners
;(.isClosed (:server-socket ss))
;(close-server ss)
;@qs
;@mbt


