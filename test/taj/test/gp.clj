(ns taj.test.gp
  (:use [taj.gp] :reload)
  (:use [clojure.test]))

(deftest test-dot-kernel
  "dot product kernel"
  (let [k dot-kernel]
    (is (= 11 (k [1 2] [3 4])) "Element-wise")
    (is (= [[5 11][11 25]] (k [[1 2] [3 4]])) "Matrix")))
