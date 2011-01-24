(ns taj.mbt
 ^{:author "Daniel Cook",
   :doc "MBTrading Quotes API"}
  (:use [clojure.java.io :only [as-url reader writer]])
  (:use [clojure.xml     :only [parse]])
  (:use [clojure.string  :only [split trim]])
  (:use [clojure.set     :only [map-invert]])
  (:require [clj-time coerce format])
  (:import (java.net Socket)))

(def *mbt* nil)         ;; connection

(declare mbt!)          ;; connect to MB trading & login
(declare status?)       ;; returns status of connection: :alive, :login-accept, :login-deny, :dead
                        ;;    if there are any errors, return that too
(declare alive?)        ;; shortcut to just find out if the connection is fully alive
(declare disconnect!)   ;; disconnect from MB trading
(declare subscribe!)    ;; subscribe to a quote stream
(declare unsubscribe!)  ;; unsubscribe from quote stream
(declare fundamental?)  ;; request fundamentals info - synchronous
(declare ping!)         ;; ping the server - synchronous, returns ping time (takes optional timeout)
(declare stream)        ;; lazy seq of incoming messages

;; Utilities

(defn mk-socket [host port] (Socket. host port))
(defn mk-writer [sock] (java.io.PrintWriter. (writer sock) true))
(defn mk-reader [sock] (reader sock))
(defn socket-open? [socket] (when socket (not (.isClosed socket))))
(defn rekey [h m]
  "Transforms the keys of a hashmap into new set of keys,
   {:k v} => {(or (m :k) :k) v}
   Leaves keys not in m intact.
   Returns nil when h is nil"
  (when h
    (let [ks (keys h)]
    (zipmap (map #(or (m %) %) ks) (map h ks)))))
(def mbt-incoming-headers
  {"G" :login-accept
   "D" :login-deny 
   "1" :level1
   "2" :level2 
   "3" :trade  
   "N" :fundamental
   "4" :option-chain 
   "9" :ping})

(def mbt-outgoing-headers
  {:login       "L"
   :subscribe   "S"
   :unsubscribe "U"
   :fundamenal  "H"
   :ping        "9"})

(def mbt-incoming-fields
  { :login-accept #{:username :msg-from}
    :login-deny   #{:username :login-reason}
    :level1       #{:symbol   :tick
                    :price    :timestamp
                    :bid      :date
                    :ask      :exchange
                    :bid-size :strike
                    :ask-size :contract-size
                    :size     :put-call
                    :close    :exp-month
                    :high     :exp-year
                    :low      :open-interest
                    :volume}
    :level2        (fn [_] true) ;; TODO specify fields
    :trade         (fn [_] true)
    :fundamental   (fn [_] true)
    :option-chain  (fn [_] true)
    :ping          #{} })

(def mbt-outgoing-fields
  { :login        #{:username :password}
    :subscribe    #{:symbol   :subs-type}
    :unsubscribe  #{:symbol   :subs-type}
    :fundamental  #{:symbol}
    :ping         #{} })

(def mbt-fields
  {"100"   :username
   "103"   :login-reason
   "1003"  :symbol
   "2000"  :subs-type 
   "2002"  :price
   "2003"  :bid
   "2004"  :ask
   "2005"  :bid-size
   "2006"  :ask-size
   "2007"  :size
   "2008"  :close
   "2009"  :high
   "2010"  :low
   "2011"  :open
   "2012"  :volume
   "2013"  :tick  
   "2014"  :time
   "2015"  :date
   "2042"  :exchange
   "2035"  :strike
   "2041"  :contract-size
   "2038"  :put-call
   "2036"  :exp-month
   "2040"  :exp-year
   "2037"  :open-interest
   "8055"  :msg-from})

(def mbt-enums
  {:level1        "20000"
   :level2        "20001"
   :level12       "20002"
   :trade         "20003"
   :option-chain  "20004"
   :uptick        "20020" 
   :downtick      "20021"})

(def mbt-enums-inv (map-invert mbt-enums))

(defn parse-integer [s]
  (try (Integer/parseInt (trim s)) 
       (catch NumberFormatException nfe 0)))

(defn parse-double [s]
  (try (Double/parseDouble (trim s)) 
       (catch NumberFormatException nfe 0.0)))

(defn parse-date [s]
  (let [s  (trim s)
        d  (clj-time.coerce/from-string s)
        d  (or d (clj-time.format/parse 
                    (clj-time.format/formatter  "MM/dd/yyyy") s))]
    (when d (clj-time.coerce/to-long d))))

(defn parse-enum [s]
  (or (mbt-enums-inv (trim s)) s))

;; Low-level interface 
(defn- get-mbt-session [user pass]
  "Obtain quote server session information from MBT login server & 
   connect socket"
  (let [url     (str "https://www.mbtrading.com/secure/getquoteserverxml.asp?"
                     "username="  user "&password=" pass)
        tree    (parse url)
        host (get-in tree [:content 0 :attrs :quote_Server])]
    [host 5020]))

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
  (.println (:writer mbt) cmd)
  mbt)

(defn- mbt-split [line]
  ^{:doc "Splits a string into a data structure
          A|B=1;C=2 into [A {B 1 C 2}]
          A         into [A nil], 
         sub routine for mbt-parse"}
  (let [head (-> line (nth 0) str trim)
        body (when (> (count line) 2) 
                  (-> (split line #"\|") second trim))
        data (when body  
              (apply hash-map 
                (vec (apply concat 
                            (map #(split % #"=") (split body #";"))))))]
    [head data]))

(defn parse-fields [msg]
  (let [to-double (filter msg 
                          [:price  :bid   :ask  :bid-size :ask-size 
                           :size   :close :high :low :open :volume
                           :strike :contract-size :open-interest])
        to-date   (filter msg 
                          [:time :date])
        to-enum   (filter msg
                          [:subs-type :tick])
        fmap (fn [m ks f]
                 (merge m (zipmap ks (map (comp f m) ks))))]
    (-> msg (fmap to-double parse-double)
            (fmap to-date   parse-date)
            (fmap to-enum   parse-enum))))

(defn mbt-parse [line]
  ^{:doc "Parses a line A|B=1;C=2 into a message"}
  (when (> (count (trim line)) 0)
    (let [[head data] (mbt-split line) 
          msg-type (or (mbt-incoming-headers head) head)
          msg-data (assoc (rekey data mbt-fields) :msg-type msg-type)]
      (parse-fields msg-data))))

(defn mbt-close [mbt]
  "Closes an MBTrading connection"
  (.close (:socket mbt))
  (assoc mbt :socket nil))

(defn mbt-ping [mbt] 
  (mbt-write mbt "9"))

;; High-level interface

(defn mbt! [user pass & server]
    (let  [[host port] (or server (get-mbt-session user pass))
           conn (-> (mbt-open user pass host port) 
                    (mbt-write (str "L|" "100=" user ";" "101=" pass "")))
           reply (-> (mbt-read conn)
                     (mbt-parse) 
                     (:msg-type))]
      (case reply 
        :login-accept  (def *mbt* (-> conn (assoc :status :alive) agent))
        :login-deny    (def *mbt* (-> conn (assoc :status :dead)  agent)))
        (:status @*mbt*)))

(defn disconnect! []
  "Disconnects from MB Trading."
  (send *mbt* mbt-close)
  (send *mbt* assoc :status :closed)
  (await *mbt*)
  (:status @*mbt*))

(defn subscribe! [subs-type symbol]
  "Subscribe to quotes"
  (send-off *mbt*
    mbt-write (str "S|" "1003=" symbol ";"
                        "2000=" (mbt-enums subs-type))))

(defn unsubscribe! [subs-type symbol]
  "Unsubscribe from quotes"
  (send-off *mbt*
    mbt-write (str "U|" "1003=" symbol ";"
                        "2000=" (mbt-enums subs-type))))

(defn fundamental? [symbol]
  "Request fundamental data"
  (send-off *mbt*
    mbt-write (str "H|" "1003=" symbol ";")))

(defn ping! []
  "Send ping message"
  (send-off *mbt* mbt-write "9"))

(defn stream []
  "Read quotes as a lazy sequence"
  (when (:reader @*mbt*)
    (filter (comp not nil?) (map mbt-parse (line-seq (:reader @*mbt*))))))

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

