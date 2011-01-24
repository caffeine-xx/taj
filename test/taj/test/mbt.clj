(ns taj.test.mbt
  (:use [taj.mbt] :reload)
  (:use [clojure.test]
        [clojure.contrib.server-socket :only [create-server close-server]])
  (:require (clojure [string :as string]))
  (:import [java.net.ServerSocket]))

;;; utilities

(def conn (promise))  ; mock qs server
(def mbt  (atom nil)) ; session
(def conn-data
  (array-map  :user "tu"
              :pass "tp"
              :host "127.0.0.1"
              :port  5029))

(defn mk-server [f]
  "Creates a server socket, and promises a connection"
  (def conn (promise))
  (let [done? (promise)
        cf    (fn [in out]
                (deliver conn {:in (mk-reader in) :out (mk-writer out)})
                (deref done?))
        ss    (create-server (:port conn-data) cf)]
    (try 
         (f)
         (Thread/sleep 0.1)
      (finally 
        (deliver done? true)
        (Thread/sleep 0.2)
        (try
          (close-server ss)
          (catch java.net.SocketException exc
            (println exc))
          (finally
            (.close ^java.net.ServerSocket (:server-socket ss))))))))

(defn sock-read []
  (let [s (.readLine (:in @conn))]
    s))

(defn sock-write [s]
  (.println (:out @conn) s))

;;; test utilities

(deftest test-rekey 
  (let [h {:a  1 :b  2}
        m {:a :A :b :B}
        n (rekey h m)]
    (is (= n {:A 1 :B 2}))))

;; test low-level api

(deftest test-parse 
  (is (= (mbt-parse "G|100=tu;8055=server1\n")  {:msg-type :login-accept :username "tu" :msg-from "server1"}))
  (is (= (mbt-parse "A|B=1;C=2\n") {:msg-type "A" "B" "1" "C" "2"}))
  (is (= (mbt-parse "A") {:msg-type "A"}))
  (is (= (mbt-parse "\n") nil)))

(deftest test-parsefields
  (is (=  {:price 205.3, :time 18938000} (parse-fields {:price "205.3" :time "05:15:38"}))))

(deftest test-open 
  (reset! mbt (apply mbt-open (vals conn-data)))
  (is (mbt-open? @mbt) "Socket connected"))

(deftest test-read
  (test-open)
  (sock-write "test data")
  (is (= "test data" (mbt-read @mbt))))

(deftest test-write
  (test-open)
  (mbt-write @mbt "test data")
  (is (= "test data" (sock-read))))

(deftest test-close
  (test-open)
  (swap! mbt mbt-close)
  (is (not (mbt-open? @mbt))))

;; test high-level api

(deftest test-login 
  (let [status (future (apply mbt! (vals conn-data)))]
    (is (= "L|100=tu;101=tp" (sock-read)))
    (sock-write "G|100=tu")
    (is (= :alive @status))))

(deftest test-login-fail
  (let [status (future (apply mbt! (vals conn-data)))]
    (is (= "L|100=tu;101=tp" (sock-read)))
    (sock-write "D|100=tu")
    (is (= :dead @status))))

(deftest test-subscribe
  (test-login)
  (subscribe! :level1 "C")
  (is (= "S|1003=C;2000=20000" (sock-read)) "Subscription message"))

(deftest test-unsubscribe
  (test-login)
  (unsubscribe! :level1 "C")
  (is (= "U|1003=C;2000=20000" (sock-read)) "Unsubscribe message"))

(defn test-ns-hook []
  (test-parse)
  (mk-server test-open)
  (mk-server test-read)
  (mk-server test-write)
  (mk-server test-close)
  (mk-server test-login)
  (mk-server test-login-fail)
  (mk-server test-subscribe)
  (mk-server test-unsubscribe))

