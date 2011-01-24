(ns taj.test.core
  (:use [taj.core] :reload)
  (:require [taj.test.mbt :as tmbt] :reload)
  (:require [taj.mbt :as mbt] :reload)
  (:use [clojure.test])
  (:use [somnium.congomongo]))

(comment (def qu-ex1 "1|2002=1.2087;2003=1.2085;2004=1.2087;2008=1.2143;2009=1.2125;2010=1.2062;2011=1.2113;2013=20021;2005=20000000;2006=24000000;2014=05:15:38;2015=01/19/2006;1003=EUR/USD\n")
(def qu-ex2 "1|2002=30.41;2003=30.41;2004=30.42;2008=30.63;2009=30.69;2010=30.34;2011=30.6;2013=20020;2007=100;2012=5638773;2005=5500;2006=14500;2014=05:18:53;2015=01/19/06;2042=PSE;1003=DELL\n")
(def msg-ex1 (mbt/mbt-parse qu-ex1))

(deftest process-time
  "Simple benchmark of parsing and insertion of messages"
  (let [N 100 M 2 mfn map
        msgs (take N (repeat qu-ex1))]
    (println "Processing " N " messages " M " times with " mfn)
    (dotimes [_ M] (time (process-quote-stream (mfn mbt/mbt-parse msgs))))))

(deftest quote-stream
  "Test to ensure parsed messages arrive into mongo db"
  (drop-database! "test-taj")
  (tmbt/test-login)
  (tmbt/sock-write qu-ex1)
  (process-quote-stream (take 1 (mbt/stream)))
  (let [fields [:symbol :high :low :open :close]
        res (map (fetch-one :stream) fields)
        ex  (map msg-ex1 fields)]
    (is (= res ex))))

(defn test-ns-hook []
  (mongo! :db "test-taj" :host "127.0.0.1" :port 27017)
  (tmbt/mk-server quote-stream)
  (process-time)))

