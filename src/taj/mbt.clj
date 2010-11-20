(ns taj.mbt
 ^{:author "Daniel Cook",
   :doc "MBTrading Quotes API"}
  (:use [clojure.java.io :only [as-url reader writer]])
  (:use [clojure.xml     :only [parse]])
  (:use [clojure.string  :only [split trim]])
  (:import java.net.Socket))

;; MBT low-level interface

(declare mbt-open)  
(declare mbt-login) 
(declare mbt-write)  
(declare mbt-read)
(declare mbt-parse)
(declare mbt-open?)
(declare mbt-close)

;; Higher-level, lazy seq-based interface using agents

(declare mbt!)          ;; connect to MB trading & login
(declare disconnect!)   ;; disconnect from MB trading
(declare subscribe!)    ;; subscribe to a quote stream
(declare unsubscribe!)  ;; unsubscribe from quote stream
(declare fundamental?)  ;; request fundamentals info - synchronous
(declare ping!)         ;; ping the server - synchronous, returns ping time (takes optional timeout)
(declare quotes)        ;; lazy seq of incoming quotes

;; Utilities

(defn mk-socket [host port] (java.net.Socket. host port))
(defn mk-writer [sock] (java.io.PrintWriter. (writer sock) true))
(defn mk-reader [sock] (reader sock))
(defn socket-open? [socket] (when socket (not (.isClosed socket))))
(defn rekey [h m]
  "Transforms the keys of a hashmap into new set of keys,
   {:k v} => {(or (m :k) :k) v}
   Leaves keys not in m intact."
  (let [ks (keys h)]
    (zipmap (map #(or (m %) %) ks) (map h ks))))

;; MBT fields

(def mbt-in-type
  {"G" :login-accept
   "D" :login-deny
   "1" :level1
   "2" :level2
   "3" :trade
   "N" :fundamental
   "4" :option-chain
   "9" :ping})

(def mbt-in-field
  {"1003"  :symbol
   "2002"  :price
   "2003"  :bid
   "2004"  :ask
   "2014"  :timestamp
   "2015"  :date})

(def mbt-out-type
  {:login       "L"
   :subscribe   "S"
   :unsubscribe "U"
   :fundamenal  "H"
   :ping        "9"})

(def mbt-subs-type
  {:level1       20000
   :level2       20001
   :level12      20002
   :trade        20003
   :option-chain 20004})

;; Low-level interface 

(defn- get-mbt-session [user pass]
  "Obtain quote server session information from MBT login server & 
   connect socket"
  (let [url     (str "https://www.mbtrading.com/secure/getquoteserverxml.asp?"
                     "username="  user "&password=" pass)
        tree    (parse url)
        session (get-in tree [:content 0 :attrs])]
    {:username user
     :password pass
     :host     (:qs_Server session)}
     :port     5020))

(defn mbt-open
  "Connect to MBTrading, return open session"
  ([user pass] (mbt-connect (get-mbt-session user pass)))
  ([session]
    (let [socket (mk-socket (:host session) (:port session))
          mbt (assoc session :socket socket
                             :reader (mk-reader socket)
                             :writer (mk-writer socket))]
      mbt)))

(defn mbt-open? [mbt]
  "Returns true if mbt socket is open"
  (socket-open? (:socket mbt)))

(defn mbt-login 
  ([mbt user pass]
    "Login to Quotes API"
    (mbt-write mbt (str "L|" "100=" user ";"
                             "101=" pass "")))
  ([mbt]
   "Login to Quotes API using session info"
   (mbt-login (:username mbt) (:password mbt))))

(defn mbt-read [mbt]
  "Reads a line from socket"
  (.readLine (:reader mbt) cmd))

(defn mbt-write [mbt cmd]
  "Writes a line directly to mbt socket"
  (.println (:writer mbt) cmd))

(defn mbt-parse [line]
  ^{:doc "Splits a string 
          A|B=1;C=2 into [A {B 1 C 2}]
          A         into [A nil]"
    :test }
  (let [head (-> s first trim)
        body (when (> (count s) 2) 
                   (-> (split s #"\|") trim second))
        msg-type (or (mbt-in-type head) head)
        data (when body  
               (apply hash-map 
                 (vec (apply concat 
                        (map #(split % #"=") (split body #";"))))))]
    [msg-type data]))

(defn mbt-close [mbt]
  "Closes an MBTrading connection"
  (when (and mbt (mbt-open? mbt))
    (.close (:socket mbt))
    (assoc mbt :socket nil)))

(defn mbt-ping [mbt] (mbt-write mbt "9"))

(defn mbt-subscribe [mbt symb qtype callback]
  "Subscribe to quotes"
  (-> (mbt-write mbt (str "S|" "1003=" symb ";"
                               "2000=" (mbt-quote-types qtype)))
      (assoc-in [:subscriptions [symb qtype]] callback)))


;; High-level interface


(declare mbt!)          ;; connect to MB trading & login
(declare disconnect!)   ;; disconnect from MB trading
(declare subscribe!)    ;; subscribe to a quote stream
(declare unsubscribe!)  ;; unsubscribe from quote stream
(declare fundamental?)  ;; request fundamentals info - synchronous
(declare ping!)         ;; ping the server - synchronous, returns ping time (takes optional timeout)
(declare quotes)        ;; lazy seq of incoming quotes

(def *mbt* nil)

(defn mbt! [user pass]
    (let  [conn (-> (mbt-connect) 
                    (mbt-login user pass))
          [reply body] (-> (mbt-read conn)
                           (mbt-parse))]
      (when (= reply :login-accept)
          (def *mbt* (-> conn (assoc :status :alive)
                               agent))
          (:status @mbt))))

