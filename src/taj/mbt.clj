(ns taj.mbt
 ^{:author "Daniel Cook",
   :doc "MBTrading Quotes API"}
  (:use [clojure.java.io :only [as-url reader writer]])
  (:use [clojure.xml     :only [parse]])
  (:use [clojure.string  :only [split trim]])
  (:import java.net.Socket))

;; MBT low-level interface

(declare mbt-open)  
(declare mbt-open?)
(declare mbt-write)  
(declare mbt-read)
(declare mbt-parse)
(declare mbt-close)

;; Higher-level, lazy seq-based interface using agents

(declare mbt!)          ;; connect to MB trading & login
(declare status?)       ;; returns status of connection: :alive, :login-accept, :login-deny, :dead
                        ;;    if there are any errors, return that too
(declare alive?)        ;; shortcut to just find out if the connection is fully alive
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
  {"100"   :username
   "103"   :login-reason
   "1003"  :symbol
   "2002"  :price
   "2003"  :bid
   "2004"  :ask
   "2014"  :timestamp
   "2015"  :date
   "8055"  :msg-from})

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
    [(:qs_Server session) 5020]))

(defn mbt-open [user pass host port]
  "Connect to MBTrading, return open session"
    (let [socket (mk-socket host port)
          mbt { :socket socket
                :reader (mk-reader socket)
                :writer (mk-writer socket)
                :host host :port  port
                :user user :pass  pass}]
      mbt))

(defn mbt-open? [mbt]
  "Returns true if mbt socket is open"
  (socket-open? (:socket mbt)))

(defn mbt-read [mbt]
  "Reads a line from socket"
  (.readLine (:reader mbt)))

(defn mbt-write [mbt cmd]
  "Writes a line directly to mbt socket"
  (.println (:writer mbt) cmd))

(defn mbt-parse [line]
  ^{:doc "Splits a string 
          A|B=1;C=2 into [A {B 1 C 2}]
          A         into [A nil]"
    :test }
  (let [head (-> line (nth 0) str trim)
        x    (println head)
        body (when (> (count line) 2) 
                   (-> (split line #"\|") trim second))
        y    (println body)
        msg-type (or (mbt-in-type head) head)
        z    (println msg-type)
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

;; High-level interface

(def *mbt* nil)

(defn mbt! [user pass & server]
    (let  [[host port] (or server (get-mbt-session user pass))
           conn (-> (mbt-open user pass host port) 
                    (mbt-write (str "L|" "100=" user ";" "101=" pass "")))
          [reply body] (-> (mbt-read conn)
                           (mbt-parse))]
      (when (= reply :login-accept)
          (def *mbt* (-> conn (assoc :status :alive)
                               agent))
          (:status @*mbt*))))

(defn disconnect! []
  "Disconnects from MB Trading."
  (send *mbt* mbt-close)
  (await *mbt*)
  (:status @*mbt*))

(defn subscribe! [subs-type symbol]
  "Subscribe to quotes"
  (send-off *mbt*
    (mbt-write (str "S|" "1003=" symbol ";"
                         "2000=" (mbt-subs-type subs-type)))))

(defn unsubscribe! [subs-type symbol]
  "Unsubscribe from quotes"
  (send-off *mbt*
      (mbt-write (str "U|" "1003=" symbol ";"
                           "2000=" (mbt-subs-type subs-type)))))

(defn fundamental? [symbol]
  "Request fundamental data"
  (send-off *mbt*
      (mbt-write (str "H|" "1003=" symbol ";"))))

(defn ping! []
  "Send ping message"
  (send-off *mbt* (mbt-write "9")))

(defn quotes []
  "Read quotes as a lazy sequence"
  (map mbt-parse (line-seq (:reader @*mbt*))))

(defn status? []
  "Checks status of MB Trading connection:
    :open
    :login-accept
    :login-deny
    :alive
    :dead
    or an exception from the agent"
  (cond 
    ((agent-error *mbt*) (agent-error *mbt*))
    ((mbt-open? @*mbt*) (:status @*mbt*))
    :otherwise :dead))

(defn alive? []
  "Shortcut to check the connection is alive"
  (and (mbt-open? @*mbt*) (= (:status @*mbt*) :alive)))

