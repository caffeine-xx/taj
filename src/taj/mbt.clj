(ns taj.mbt
  (:use [clojure.java.io :only [as-url reader writer]])
  (:use [clojure.xml     :only [parse]])
  (:use [clojure.string  :only [split trim]])
  (:import java.net.Socket))

;; MBT Interface
(declare mbt-connect)
(declare mbt-subscribe)
(declare mbt-unsubscribe)
(declare mbt-fundamental)
(declare mbt-ping)
(declare mbt-close)
(declare mbt-open?)
(declare mbt-read)

(defn mk-socket [host port] (java.net.Socket. host port))
(defn mk-writer [sock] (java.io.PrintWriter. (writer sock) true))
(defn mk-reader [sock] (reader sock))
(defn socket-open? [socket] (when socket (not (.isClosed socket))))
(defn rekey [h m]
  "Transforms the keys of a hashmap into new set of keys,
   {:k v} => {(m :k) v}
   Ignores keys not in m"
  (let [ks (keys h)]
    (zipmap (map m ks) (map h ks))))

(def mbt-quote-types 
  {:level1  20000
   :level2  20001
   :level12 20002
   :trades  20003
   :chains  20004})

(def mbt-quote-fields
  {"1003"  :symbol
   "2002"  :price
   "2003"  :bid
   "2004"  :ask
   "2014"  :timestamp
   "2015"  :date})

(defn split-msg [s]
  ^{:doc "Splits a string 
          A|B=1;C=2 into [A {B 1 C 2}]
          A         into [A nil]"
    :test }
  (if (> (count s) 2)
    (let [[head body] (map trim (split s #"\|"))
           data (apply hash-map 
                  (vec (apply concat 
                    (map #(split % #"=") (split body #";")))))]
      [head data])
    [(trim (first s)) nil]))

(defn mbt-write [mbt cmd]
  "Writes a line directly to mbt socket"
  (.println (:writer mbt) cmd)
  (println "mbt-write: " cmd)
  mbt)

(defn mbt-parse [mbt head data]
    (let [data     (rekey data mbt-quote-fields)
          qtype    ({"1" :level1} head)
          cbkey    [(data :symbol) qtype]
          callback (get-in mbt [:subscriptions cbkey])]
      (when callback
        (callback data))
      mbt))

(def mbt-msg-handlers
  {"G" (fn [mbt head data] (assoc mbt :status :alive))
   "D" (fn [mbt head data] (assoc mbt :status :denied))
   "9" (fn [mbt head data] (println "ping!") mbt)
   "1" mbt-parse})

(defn mbt-handler [mbt line]
  "Dispatch a line to a handler"
  (let [[head data] (split-msg line) 
        handler     (or (mbt-msg-handlers head) 
                        (fn [mbt head data] 
                          (println "unknown :" head ":") 
                          mbt))]
    (handler mbt head data)))

(defn mbt-read [mbt]
  "Synchronously reads a line from the socket, and calls the handler"
  (when (and (mbt-open? mbt) (:reader mbt))
    (let [line (.readLine (:reader mbt))]
      (mbt-handler mbt line))))

(defn mbt-open? [mbt]
  "Returns true if mbt socket is open"
  (socket-open? (:socket mbt)))

(defn mbt-login [mbt]
  "Login to Quotes API"
  (mbt-write mbt (str "L|" "100=" (:username mbt) ";"
                           "101=" (:password mbt) "")))

(defn get-mbt-session [user pass]
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

(defn mbt-connect
  "Connect to MBTrading, return a quote-stream agent"
  ([user pass] (mbt-connect (get-mbt-session user pass)))
  ([session]
    (let [socket (mk-socket (:host session) (:port session))
          mbt (assoc session :socket socket
                             :reader (mk-reader socket)
                             :writer (mk-writer socket))]
      mbt)))

(defn mbt-ping [mbt] (mbt-write mbt "9"))

(defn mbt-subscribe [mbt symb qtype callback]
  "Subscribe to quotes"
  (-> (mbt-write mbt (str "S|" "1003=" symb ";"
                               "2000=" (mbt-quote-types qtype)))
      (assoc-in [:subscriptions [symb qtype]] callback)))
 
(defn mbt-close [mbt]
  "Closes an MBTrading connection"
  (when (and mbt (mbt-open? mbt))
    (.close (:socket mbt))
    (assoc mbt :socket nil)))

;(def mysock (mbt-connect {:host "127.0.0.1" :port 5020 :username "a" :password "b"}))
;(mbt-close mysock)

