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
  #{:user "tu"
    :pass "tp"
    :host "127.0.0.1"
    :port  5020})

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
    (reset! mbt (apply mbt-open (vals conn-data)))
    (f)
    (swap! mbt mbt-close)
    (deliver done? true)
    (close-server ss)
    (Thread/sleep 0.2) ;dodgy
    (deref done?)))    ;hack.

(defn sock-read []
  (println "sock-read conn: " (:in @conn))
  (let [s (.readLine (:in @conn))]
    (println "sock-read:" s)
    s))

(defn sock-write [s]
  (.println (:out @conn) s)
  (println "sock-write:" s))

;;; test utilities

(deftest test-rekey 
  (let [h {:a  1 :b  2}
        m {:a :A :b :B}
        n (rekey h m)]
    (is (= n {:A 1 :B 2}))))

;; test low-level api

(deftest test-parse 
  (is (= (mbt-parse "G|100=tu;8055=server1\n") [:login-accept {:username "tu" :msg-from "server1"}]))
  (is (= (mbt-parse "A|B=1;C=2\n") ["A" {"B" "1" "C" "2"}]))
  (is (= (mbt-parse "A") ["A" nil])))

(deftest test-open 
  (println "test-connect")
  (mk-mbt-conn 
    (fn [] (is (mbt-open? @mbt) "Socket connected"))))

(deftest test-read
  (println "test-read")
  (mk-mbt-conn 
    #((sock-write "test data")
      (is (= "test data" (mbt-read @mbt))))))

(deftest test-write
  (println "test-write")
  (mk-mbt-conn 
    #((swap! mbt mbt-write "test data")
      (is (= "test data" (sock-read))))))

;; (deftest test-login-success
;;   (println "test-login-success")
;;   (mk-mbt-conn
;;     (fn [] 
;;       (swap! mbt mbt-login)
;;       (is (= "L|100=tu;101=tp" (sock-read)) "Login message")
;;       (sock-write "G|100=tu") 
;;       (swap! mbt mbt-read)
;;       (is (= :alive (:status @mbt)) "Good Login"))))
;; 
;; (deftest test-login-denied
;;   (println "test-login-denied")
;;   (mk-mbt-conn
;;     (fn [] 
;;       (swap! mbt mbt-login)
;;       (is (= "L|100=tu;101=tp" (sock-read)) "Login message")
;;       (sock-write "D|100=tu;103=Failz") 
;;       (swap! mbt mbt-read)
;;       (is (= :denied (:status @mbt)) "Login denied"))))
;; 
;; (deftest test-subscribe
;;   (println "test-subscribe")
;;   (mk-mbt-conn
;;     (fn [] 
;;       (let [recv? (promise)]
;;         (swap! mbt 
;;                mbt-subscribe "C" :level1 
;;                             (fn [data] 
;;                               (is (= {:symbol "C" :price "1.5"} data) "Quote parsed properly")
;;                               (deliver recv? true)))
;;         (is (= "S|1003=C;2000=20000" (sock-read)) "Correct subscription message")
;;         (sock-write "1|1003=C;2002=1.5")
;;         (swap! mbt mbt-read)
;;         (is (= @recv? true) "Callback actually got called")))))

(defn test-ns-hook []
  (test-parse)
;;  (test-mbt-ping)
;;  (test-connect)
;;  (test-login-success)
;;  (test-login-denied)
;;  (test-subscribe)
  )
